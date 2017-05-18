module LambdaCube.WebGL.Input where

import Prelude
import Data.List as List
import Data.Map as Map
import Data.ArrayBuffer.Types as AB
import Data.StrMap (StrMap, empty, fromFoldable, keys, lookup, size, toUnfoldable, union, values) as StrMap
import Data.StrMap.Unsafe (unsafeIndex) as StrMap
import Graphics.WebGLRaw as GL
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, newRef, readRef, writeRef)
import Data.Array (concat, concatMap, length, replicate, sortBy, unsafeIndex, unzip, updateAt)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (foldl, for, for_, traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import LambdaCube.IR (InputType, Pipeline(..), Slot(..))
import LambdaCube.LinearBase (Bool, Float, Int32, M22F, M33F, M44F, V2(..), V2B, V2F, V2I, V2U, V3B, V3F, V3I, V4B, V4F, V4I)
import LambdaCube.PipelineSchema (ObjectArraySchema(..), PipelineSchema(..), StreamType(..))
import LambdaCube.WebGL.Type (Buffer, GFX, GLObject, GLObjectCommand(..), GLProgram, GLUniform, IndexStream, InputConnection(..), InputSetter(..), OrderJob(..), Primitive, SetterFun, Stream(..), TextureData, WebGLPipelineInput, sizeOfArrayType, streamToStreamType)
import LambdaCube.WebGL.Util (arrayTypeToGLType, mkUniformSetter, primitiveToFetchPrimitive, primitiveToGLType, toStreamType, unlines)
import Partial.Unsafe (unsafePartial)

-- API
schemaFromPipeline :: Pipeline -> GFX PipelineSchema
schemaFromPipeline (Pipeline ppl) = do
  sl <- for ppl.slots $ \(Slot s) -> do
    a <- traverse toStreamType s.slotStreams
    pure $ Tuple s.slotName $ ObjectArraySchema {primitive: s.slotPrimitive, attributes: a}
  let ul = map (\(Slot s) -> s.slotUniforms) ppl.slots
  pure $ PipelineSchema
    { objectArrays: StrMap.fromFoldable sl
    , uniforms: foldl StrMap.union (StrMap.empty :: StrMap.StrMap InputType) ul
    }

mkUniform :: Array (Tuple String InputType) -> GFX (Tuple (StrMap.StrMap InputSetter) (StrMap.StrMap GLUniform))
mkUniform l = do
  unisAndSetters <- for l $ \(Tuple n t) -> do
    (Tuple uni setter) <- mkUniformSetter t
    pure $ Tuple (Tuple n uni) (Tuple n setter)
  let fun (Tuple unis setters) = Tuple (StrMap.fromFoldable setters) (StrMap.fromFoldable unis)
  pure $ fun $ unzip unisAndSetters

mkWebGLPipelineInput :: PipelineSchema -> GFX WebGLPipelineInput
mkWebGLPipelineInput (PipelineSchema sch) = unsafePartial $ do
  let sm = StrMap.fromFoldable $ List.zip (List.fromFoldable $ StrMap.keys sch.objectArrays) (List.range 0 len)
      len = StrMap.size sch.objectArrays
  Tuple setters unis <- mkUniform $ StrMap.toUnfoldable sch.uniforms
  slotV <- replicateA len $ newRef {objectMap: Map.empty :: Map.Map Int GLObject, sortedObjects: [], orderJob: Ordered}
  seed <- newRef 0
  size <- newRef (V2 0 0)
  ppls <- newRef [Nothing]
  pure $
    { schema        : PipelineSchema sch
    , slotMap       : sm
    , slotVector    : slotV
    , objSeed       : seed
    , uniformSetter : setters
    , uniformSetup  : unis
    , screenSize    : size
    , pipelines     : ppls
    }

addObject :: WebGLPipelineInput -> String -> Primitive -> Maybe (IndexStream (Buffer AB.Int32)) -> StrMap.StrMap (Stream (Buffer AB.Float32)) -> Array String -> GFX GLObject
addObject input slotName prim indices attribs uniformNames = unsafePartial $ do
    PipelineSchema schema <- pure input.schema
    for_ uniformNames $ \n -> case StrMap.lookup n schema.uniforms of
        Nothing -> throwException $ error $ "Unknown uniform: " <> show n
        _ -> pure unit
    case StrMap.lookup slotName schema.objectArrays of
        Nothing -> throwException $ error $ "Unknown objectArray: " <> slotName
        Just (ObjectArraySchema slotSchema) -> do
            when (slotSchema.primitive /= (primitiveToFetchPrimitive prim)) $ throwException $ error $
                "Primitive mismatch for slot (" <> show slotName <> ") expected " <> show slotSchema.primitive  <> " but got " <> show prim
            let sType = streamToStreamType <$> attribs
            when (sType /= slotSchema.attributes) $ throwException $ error $ unlines $ 
                [ "Attribute stream mismatch for slot (" <> show slotName <> ") expected "
                , show slotSchema.attributes
                , " but got "
                , show sType
                ]

    slotIdx <- case slotName `StrMap.lookup` input.slotMap of
      Nothing -> throwException $ error "internal error (slot index)"
      Just i  -> pure i
    order <- newRef 0
    enabled <- newRef true
    index <- readRef input.objSeed
    modifyRef input.objSeed (\x -> 1+x)
    Tuple setters unis <- mkUniform =<< (for uniformNames $ \n -> case StrMap.lookup n schema.uniforms of
      Nothing -> throwException $ error "internal error (uniform setter not found)"
      Just t -> pure $ Tuple n t)
    cmdsRef <- newRef [[]]
    let obj =
          { slot       : slotIdx
          , primitive  : prim
          , indices    : indices
          , attributes : attribs
          , uniSetter  : setters
          , uniSetup   : unis
          , order      : order
          , enabled    : enabled
          , id         : index
          , commands   : cmdsRef
          }

    modifyRef (input.slotVector `unsafeIndex` slotIdx) $ \s -> {objectMap: Map.insert index obj s.objectMap, sortedObjects: [], orderJob: Generate}

    -- generate GLObjectCommands for the new object
    {-
        foreach pipeline:
            foreach realted program:
                generate commands
    -}
    ppls <- readRef input.pipelines
    cmds <- for ppls $ \mp -> case mp of
        Nothing -> pure []
        Just p  -> do
            Just (InputConnection ic) <- readRef p.input
            case ic.slotMapInputToPipeline `unsafeIndex` slotIdx of
                Nothing -> do
                    pure []   -- this slot is not used in that pipeline
                Just pSlotIdx -> do
                    let emptyV = replicate (length p.programs) []
                        addCmds v prgIdx = fromJust $ updateAt prgIdx (createObjectCommands p.texUnitMapping input.uniformSetup obj (p.programs `unsafeIndex` prgIdx)) v
                    pure $ foldl addCmds emptyV $ p.slotPrograms `unsafeIndex` pSlotIdx
    writeRef cmdsRef cmds
    pure obj

removeObject :: WebGLPipelineInput -> GLObject -> GFX Unit
removeObject p obj = modifyRef (unsafePartial $ p.slotVector `unsafeIndex` obj.slot) $ \s -> {objectMap:Map.delete obj.id s.objectMap, sortedObjects:[], orderJob:Generate}

enableObject :: GLObject -> Bool -> GFX Unit
enableObject obj b = writeRef obj.enabled b

setObjectOrder :: WebGLPipelineInput -> GLObject -> Int -> GFX Unit
setObjectOrder p obj i = do
    writeRef obj.order i
    modifyRef (unsafePartial $ p.slotVector `unsafeIndex` obj.slot) $ \s -> s {orderJob = Reorder}

objectUniformSetter :: GLObject -> StrMap.StrMap InputSetter
objectUniformSetter o = o.uniSetter

setScreenSize :: WebGLPipelineInput -> V2U -> GFX Unit
setScreenSize p s = writeRef p.screenSize s

sortSlotObjects :: WebGLPipelineInput -> GFX Unit
sortSlotObjects p = do
  for_ p.slotVector $ \slotRef -> do
    slot <- readRef slotRef
    let cmpFun (Tuple a _) (Tuple b _) = a `compare` b
        doSort objs = writeRef slotRef $ slot {sortedObjects = sortBy cmpFun objs, orderJob = Ordered}
    case slot.orderJob of
        Ordered -> pure unit
        Generate -> do
            objs <- for (Map.values slot.objectMap) $ \obj -> do
                ord <- readRef obj.order
                pure $ Tuple ord obj
            doSort $ List.toUnfoldable objs
        Reorder -> do
            objs <- for slot.sortedObjects $ \(Tuple _ obj) -> do
                ord <- readRef obj.order
                pure (Tuple ord obj)
            doSort objs

nullSetter :: forall a . String -> String -> a -> GFX Unit
nullSetter n t _ = pure unit -- Prelude.putStrLn $ "WARNING: unknown uniform: " <> n <> " :: " <> t

uniformBool :: String -> StrMap.StrMap InputSetter -> SetterFun Bool
uniformBool n is =
  case StrMap.lookup n is of
  Just (SBool fun) -> fun
  _ -> nullSetter n "Bool"

uniformV2B :: String -> StrMap.StrMap InputSetter -> SetterFun V2B
uniformV2B n is = case StrMap.lookup n is of
  Just (SV2B fun) -> fun
  _ -> nullSetter n "V2B"

uniformV3B :: String -> StrMap.StrMap InputSetter -> SetterFun V3B
uniformV3B n is = case StrMap.lookup n is of
  Just (SV3B fun) -> fun
  _ -> nullSetter n "V3B"

uniformV4B :: String -> StrMap.StrMap InputSetter -> SetterFun V4B
uniformV4B n is =case StrMap.lookup n is of
  Just (SV4B fun) -> fun
  _ -> nullSetter n "V4B"

uniformInt :: String -> StrMap.StrMap InputSetter -> SetterFun Int32
uniformInt n is = case StrMap.lookup n is of
  Just (SInt fun) -> fun
  _ -> nullSetter n "Int"

uniformV2I :: String -> StrMap.StrMap InputSetter -> SetterFun V2I
uniformV2I n is = case StrMap.lookup n is of
  Just (SV2I fun) -> fun
  _ -> nullSetter n "V2I"

uniformV3I :: String -> StrMap.StrMap InputSetter -> SetterFun V3I
uniformV3I n is = case StrMap.lookup n is of
  Just (SV3I fun) -> fun
  _ -> nullSetter n "V3I"

uniformV4I :: String -> StrMap.StrMap InputSetter -> SetterFun V4I
uniformV4I n is = case StrMap.lookup n is of
  Just (SV4I fun) -> fun
  _ -> nullSetter n "V4I"

uniformFloat :: String -> StrMap.StrMap InputSetter -> SetterFun Float
uniformFloat n is = case StrMap.lookup n is of
  Just (SFloat fun) -> fun
  _ -> nullSetter n "Float"

uniformV2F :: String -> StrMap.StrMap InputSetter -> SetterFun V2F
uniformV2F n is = case StrMap.lookup n is of
  Just (SV2F fun) -> fun
  _ -> nullSetter n "V2F"

uniformV3F :: String -> StrMap.StrMap InputSetter -> SetterFun V3F
uniformV3F n is = case StrMap.lookup n is of
  Just (SV3F fun) -> fun
  _ -> nullSetter n "V3F"

uniformV4F :: String -> StrMap.StrMap InputSetter -> SetterFun V4F
uniformV4F n is = case StrMap.lookup n is of
  Just (SV4F fun) -> fun
  _ -> nullSetter n "V4F"

uniformM22F :: String -> StrMap.StrMap InputSetter -> SetterFun M22F
uniformM22F n is = case StrMap.lookup n is of
  Just (SM22F fun) -> fun
  _ -> nullSetter n "M22F"

uniformM33F :: String -> StrMap.StrMap InputSetter -> SetterFun M33F
uniformM33F n is = case StrMap.lookup n is of
  Just (SM33F fun) -> fun
  _ -> nullSetter n "M33F"

uniformM44F :: String -> StrMap.StrMap InputSetter -> SetterFun M44F
uniformM44F n is = case StrMap.lookup n is of
  Just (SM44F fun) -> fun
  _ -> nullSetter n "M44F"

uniformFTexture2D :: String -> StrMap.StrMap InputSetter -> SetterFun TextureData
uniformFTexture2D n is = case StrMap.lookup n is of
  Just (SFTexture2D fun) -> fun
  _ -> nullSetter n "FTexture2D"

createObjectCommands :: StrMap.StrMap (Ref Int) -> StrMap.StrMap GLUniform -> GLObject -> GLProgram -> Array GLObjectCommand
createObjectCommands texUnitMap topUnis obj prg = concat [objUniCmds, objStreamCmds, [objDrawCmd]]
  where
    -- object draw command
    objDrawCmd = unsafePartial $ let
        prim = primitiveToGLType obj.primitive
        streamLen a = case a of
          Stream s  -> [s.length]
          _         -> []
        count = (concatMap streamLen $ StrMap.values obj.attributes) `unsafeIndex` 0
      in case obj.indices of
        Nothing -> GLDrawArrays prim 0 count
        Just is -> let 
            a = is.buffer.arrays `unsafeIndex` is.arrIdx
            idxType = arrayTypeToGLType a.arrType
            ptr    = a.arrOffset + is.start * sizeOfArrayType a.arrType
          in GLDrawElements prim is.length idxType is.buffer.glBuffer ptr

    -- object uniform commands
    -- texture slot setup commands
    objUniCmds = unsafePartial $ uniCmds `append` texCmds
      where
        topUni n = StrMap.unsafeIndex topUnis n
        uniMap  = StrMap.toUnfoldable prg.inputUniforms
        uniCmds = flip map uniMap $ \(Tuple n i) -> GLSetUniform i $ case StrMap.lookup n obj.uniSetup of
          Nothing -> topUnis `StrMap.unsafeIndex` n
          Just u  -> u
        texCmds = flip map prg.inputTextureUniforms $ \n ->
          let u = case StrMap.lookup n obj.uniSetup of
                    Nothing -> topUni n
                    Just a -> a
              texUnit = StrMap.unsafeIndex texUnitMap n
              txTarget = GL._TEXTURE_2D -- TODO
          in GLBindTexture txTarget texUnit u

    -- object attribute stream commands
    objStreamCmds = unsafePartial $ flip map (StrMap.values prg.inputStreams) $ \is -> let
        x = obj.attributes `StrMap.unsafeIndex` is.slotAttribute
        i = is.location
      in case x of
          Stream s -> let
              desc = s.buffer.arrays `unsafeIndex` s.arrIdx
              glType      = arrayTypeToGLType desc.arrType
              ptr compCnt = desc.arrOffset + s.start * compCnt * sizeOfArrayType desc.arrType
              setFloatAttrib n = GLSetVertexAttribArray i s.buffer.glBuffer n glType (ptr n)
            in setFloatAttrib $ case s.sType of
                Attribute_Float  -> 1
                Attribute_V2F    -> 2
                Attribute_V3F    -> 3
                Attribute_V4F    -> 4
                Attribute_M22F   -> 4
                Attribute_M33F   -> 9
                Attribute_M44F   -> 16
            -- constant generic attribute
          constAttr -> GLSetVertexAttrib i constAttr
