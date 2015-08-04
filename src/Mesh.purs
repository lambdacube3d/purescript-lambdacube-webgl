module Mesh where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Exception

import qualified Data.StrMap as StrMap
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array
import Data.Tuple
import Data.Int
import qualified Data.List as List

import IR
import Type
import Data
import Input

data AttributeType
    = AT_Float
    | AT_V2F
    | AT_V3F
    | AT_V4F
    | AT_M22F
    | AT_M33F
    | AT_M44F

data MeshAttribute
    = A_Float   (Array Float)
    | A_V2F     (Array V2F)
    | A_V3F     (Array V3F)
    | A_V4F     (Array V4F)
    | A_M22F    (Array M22F)
    | A_M33F    (Array M33F)
    | A_M44F    (Array M44F)
    | A_Flat    AttributeType (Array Float)

data MeshPrimitive
    = P_Points
    | P_TriangleStrip
    | P_Triangles
    | P_TriangleStripI  (Array Word16)
    | P_TrianglesI      (Array Word16)

type Mesh =
    { attributes  :: StrMap.StrMap MeshAttribute
    , primitive   :: MeshPrimitive
    , gpuData     :: Maybe GPUData
    }

type GPUData =
    { primitive :: Primitive
    , streams   :: StrMap.StrMap (Stream Buffer)
    , indices   :: Maybe (IndexStream Buffer)
    }

addMesh :: WebGLPipelineInput -> String -> Mesh -> Array String -> GFX GLObject
addMesh input slotName mesh objUniNames = case mesh.gpuData of
  Nothing -> throwException $ error "addMesh: only compiled mesh with GPUData is supported"
  Just g -> case StrMap.lookup slotName input.schema.slots of
    Nothing -> throwException $ error "addMesh: slot not found"
    Just slotSchema -> do
      -- select proper attributes
      let filterStream (Tuple n s) = StrMap.member n slotSchema.attributes
      addObject input slotName g.primitive g.indices (StrMap.fromList $ List.filter filterStream $ StrMap.toList g.streams) objUniNames

compileMesh :: Mesh -> GFX Mesh
compileMesh mesh = case mesh.gpuData of
  Just _ -> return mesh
  Nothing -> do
    let mkIndexBuf v = do
            iBuf <- compileBuffer [Array ArrWord16 (toArray v)]
            return $ Just {buffer: iBuf, arrIdx: 0, start: 0, length: length v}
    vBuf <- compileBuffer $ map meshAttrToArray $ List.fromList (StrMap.values mesh.attributes)
    Tuple prim indices <- case mesh.primitive of
        P_Points            -> return $ Tuple PointList     Nothing
        P_TriangleStrip     -> return $ Tuple TriangleStrip Nothing
        P_Triangles         -> return $ Tuple TriangleList  Nothing
        P_TriangleStripI v  -> Tuple TriangleStrip <$> mkIndexBuf v
        P_TrianglesI v      -> Tuple TriangleList <$> mkIndexBuf v
    let streams = StrMap.fromList $ List.zipWith (\i (Tuple n a) -> Tuple n (meshAttrToStream vBuf i a)) (List.range 0 $ fromJust $ fromNumber $ StrMap.size mesh.attributes) (StrMap.toList mesh.attributes)
        gpuData = {primitive: prim, streams: streams, indices: indices}
    return $ mesh {gpuData = Just gpuData}

meshAttrToArray :: MeshAttribute -> LCArray
meshAttrToArray a = case a of
  A_Float v   -> Array ArrFloat $ toArray v
  A_V2F   v   -> Array ArrFloat $ toArray v
  A_V3F   v   -> Array ArrFloat $ toArray v
  A_V4F   v   -> Array ArrFloat $ toArray v
  A_M22F  v   -> Array ArrFloat $ toArray v
  A_M33F  v   -> Array ArrFloat $ toArray v
  A_M44F  v   -> Array ArrFloat $ toArray v
  A_Flat _ v  -> Array ArrFloat v

meshAttrToStream :: Buffer -> Int -> MeshAttribute -> Stream Buffer
meshAttrToStream b i a = Stream $ case a of
  A_Float v   -> {sType: TFloat, buffer: b, arrIdx: i , start: 0, length: length v}
  A_V2F   v   -> {sType: TV2F  , buffer: b, arrIdx: i , start: 0, length: length v}
  A_V3F   v   -> {sType: TV3F  , buffer: b, arrIdx: i , start: 0, length: length v}
  A_V4F   v   -> {sType: TV4F  , buffer: b, arrIdx: i , start: 0, length: length v}
  A_M22F  v   -> {sType: TM22F , buffer: b, arrIdx: i , start: 0, length: length v}
  A_M33F  v   -> {sType: TM33F , buffer: b, arrIdx: i , start: 0, length: length v}
  A_M44F  v   -> {sType: TM44F , buffer: b, arrIdx: i , start: 0, length: length v}
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
