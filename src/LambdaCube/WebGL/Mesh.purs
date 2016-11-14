module LambdaCube.WebGL.Mesh where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Exception

import Data.StrMap as StrMap
import Data.Maybe
import Data.Maybe (fromJust)
import Data.Array
import Data.Tuple
import Data.Int
import Data.List as List

import Partial.Unsafe (unsafeCrashWith, unsafePartial)

import LambdaCube.Mesh
import LambdaCube.IR
import LambdaCube.LinearBase
import LambdaCube.WebGL.Type
import LambdaCube.WebGL.Data
import LambdaCube.WebGL.Input

type GPUData =
    { primitive :: Primitive
    , streams   :: StrMap.StrMap (Stream Buffer)
    , indices   :: Maybe (IndexStream Buffer)
    }

data GPUMesh = GPUMesh
    { meshData  :: Mesh
    , gpuData   :: GPUData
    }

addMesh :: WebGLPipelineInput -> String -> GPUMesh -> Array String -> GFX GLObject
addMesh input slotName (GPUMesh mesh) objUniNames = case StrMap.lookup slotName input.schema.slots of
    Nothing -> throwException $ error "addMesh: slot not found"
    Just slotSchema -> do
      -- select proper attributes
      let filterStream (Tuple n s) = StrMap.member n slotSchema.attributes
      addObject input slotName mesh.gpuData.primitive mesh.gpuData.indices (StrMap.fromFoldable $ List.filter filterStream $ StrMap.toList mesh.gpuData.streams) objUniNames

compileMesh :: Mesh -> GFX GPUMesh
compileMesh (Mesh mesh) = unsafePartial $ do
    let mkIndexBuf v = do
            iBuf <- compileBuffer [Array ArrWord16 (toArray v)]
            pure $ Just {buffer: iBuf, arrIdx: 0, start: 0, length: length v}
    vBuf <- compileBuffer $ map meshAttrToArray $ List.toUnfoldable (StrMap.values mesh.mAttributes)
    Tuple prim indices <- case mesh.mPrimitive of
        P_Points            -> pure $ Tuple PointList     Nothing
        P_TriangleStrip     -> pure $ Tuple TriangleStrip Nothing
        P_Triangles         -> pure $ Tuple TriangleList  Nothing
        P_TriangleStripI v  -> Tuple TriangleStrip <$> mkIndexBuf v
        P_TrianglesI v      -> Tuple TriangleList <$> mkIndexBuf v
    let streams = StrMap.fromFoldable $ List.zipWith (\i (Tuple n a) -> Tuple n (meshAttrToStream vBuf i a))
                                                     (List.range 0 $ fromJust $ fromNumber $ StrMap.size mesh.mAttributes)
                                                     (StrMap.toList mesh.mAttributes)
        gpuData = {primitive: prim, streams: streams, indices: indices}
    pure $ GPUMesh {meshData: Mesh mesh, gpuData: gpuData}

meshAttrToArray :: MeshAttribute -> LCArray
meshAttrToArray a = case a of
  A_Float v   -> Array ArrFloat $ toArray v
  A_V2F   v   -> Array ArrFloat $ toArray v
  A_V3F   v   -> Array ArrFloat $ toArray v
  A_V4F   v   -> Array ArrFloat $ toArray v
  A_M22F  v   -> Array ArrFloat $ toArray v
  A_M33F  v   -> Array ArrFloat $ toArray v
  A_M44F  v   -> Array ArrFloat $ toArray v
  _ -> unsafeCrashWith "meshAttrToArray - unsupported MeshAttribute"
{-
  A_Flat _ v  -> Array ArrFloat v
-}
meshAttrToStream :: Buffer -> Int -> MeshAttribute -> Stream Buffer
meshAttrToStream b i a = Stream $ case a of
  A_Float v   -> {sType: TFloat, buffer: b, arrIdx: i , start: 0, length: length v}
  A_V2F   v   -> {sType: TV2F  , buffer: b, arrIdx: i , start: 0, length: length v}
  A_V3F   v   -> {sType: TV3F  , buffer: b, arrIdx: i , start: 0, length: length v}
  A_V4F   v   -> {sType: TV4F  , buffer: b, arrIdx: i , start: 0, length: length v}
  A_M22F  v   -> {sType: TM22F , buffer: b, arrIdx: i , start: 0, length: length v}
  A_M33F  v   -> {sType: TM33F , buffer: b, arrIdx: i , start: 0, length: length v}
  A_M44F  v   -> {sType: TM44F , buffer: b, arrIdx: i , start: 0, length: length v}
  _ -> unsafeCrashWith "meshAttrToStream - unsupported MeshAttribute"
{-
  A_Flat t v  -> let
      tn = case t of
        AT_Float  -> Tuple TFloat 1
        AT_V2F    -> Tuple TV2F   2
        AT_V3F    -> Tuple TV3F   3
        AT_V4F    -> Tuple TV4F   4
        AT_M22F   -> Tuple TM22F  4
        AT_M33F   -> Tuple TM33F  9
        AT_M44F   -> Tuple TM44F  16
    in case tn of
      Tuple st n -> {sType: st , buffer: b, arrIdx: i , start: 0, length: length v / n}
-}