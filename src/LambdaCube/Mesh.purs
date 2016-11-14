-- generated file, do not modify!
-- 2016-11-14T21:30:03.056917000000Z

module LambdaCube.Mesh where
import Prelude
import Data.Generic
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap(..))
import Data.Map (Map(..))
import Data.List (List(..))
import LambdaCube.LinearBase

import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)


data MeshAttribute
  = A_Float (Array Float)
  | A_V2F (Array V2F)
  | A_V3F (Array V3F)
  | A_V4F (Array V4F)
  | A_M22F (Array M22F)
  | A_M33F (Array M33F)
  | A_M44F (Array M44F)
  | A_Int (Array Int32)
  | A_Word (Array Word32)

data MeshPrimitive
  = P_Points
  | P_TriangleStrip
  | P_Triangles
  | P_TriangleStripI (Array Int32)
  | P_TrianglesI (Array Int32)

data Mesh
  = Mesh
  { mAttributes :: StrMap MeshAttribute
  , mPrimitive :: MeshPrimitive
  }




instance encodeJsonMeshAttribute :: EncodeJson MeshAttribute where
  encodeJson v = case v of
    A_Float arg0 -> "tag" := "A_Float" ~> "arg0" := arg0 ~> jsonEmptyObject
    A_V2F arg0 -> "tag" := "A_V2F" ~> "arg0" := arg0 ~> jsonEmptyObject
    A_V3F arg0 -> "tag" := "A_V3F" ~> "arg0" := arg0 ~> jsonEmptyObject
    A_V4F arg0 -> "tag" := "A_V4F" ~> "arg0" := arg0 ~> jsonEmptyObject
    A_M22F arg0 -> "tag" := "A_M22F" ~> "arg0" := arg0 ~> jsonEmptyObject
    A_M33F arg0 -> "tag" := "A_M33F" ~> "arg0" := arg0 ~> jsonEmptyObject
    A_M44F arg0 -> "tag" := "A_M44F" ~> "arg0" := arg0 ~> jsonEmptyObject
    A_Int arg0 -> "tag" := "A_Int" ~> "arg0" := arg0 ~> jsonEmptyObject
    A_Word arg0 -> "tag" := "A_Word" ~> "arg0" := arg0 ~> jsonEmptyObject

instance decodeJsonMeshAttribute :: DecodeJson MeshAttribute where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "A_Float" -> A_Float <$> obj .? "arg0"
      "A_V2F" -> A_V2F <$> obj .? "arg0"
      "A_V3F" -> A_V3F <$> obj .? "arg0"
      "A_V4F" -> A_V4F <$> obj .? "arg0"
      "A_M22F" -> A_M22F <$> obj .? "arg0"
      "A_M33F" -> A_M33F <$> obj .? "arg0"
      "A_M44F" -> A_M44F <$> obj .? "arg0"
      "A_Int" -> A_Int <$> obj .? "arg0"
      "A_Word" -> A_Word <$> obj .? "arg0"
      _ -> Left ("decodeJsonMeshAttribute - unknown tag: " <> tag)

instance encodeJsonMeshPrimitive :: EncodeJson MeshPrimitive where
  encodeJson v = case v of
    P_Points -> "tag" := "P_Points" ~> jsonEmptyObject
    P_TriangleStrip -> "tag" := "P_TriangleStrip" ~> jsonEmptyObject
    P_Triangles -> "tag" := "P_Triangles" ~> jsonEmptyObject
    P_TriangleStripI arg0 -> "tag" := "P_TriangleStripI" ~> "arg0" := arg0 ~> jsonEmptyObject
    P_TrianglesI arg0 -> "tag" := "P_TrianglesI" ~> "arg0" := arg0 ~> jsonEmptyObject

instance decodeJsonMeshPrimitive :: DecodeJson MeshPrimitive where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "P_Points" -> pure P_Points
      "P_TriangleStrip" -> pure P_TriangleStrip
      "P_Triangles" -> pure P_Triangles
      "P_TriangleStripI" -> P_TriangleStripI <$> obj .? "arg0"
      "P_TrianglesI" -> P_TrianglesI <$> obj .? "arg0"
      _ -> Left ("decodeJsonMeshPrimitive - unknown tag: " <> tag)

instance encodeJsonMesh :: EncodeJson Mesh where
  encodeJson v = case v of
    Mesh r ->
      "tag" := "Mesh" ~>
      "mAttributes" := r.mAttributes ~>
      "mPrimitive" := r.mPrimitive ~>
      jsonEmptyObject

instance decodeJsonMesh :: DecodeJson Mesh where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Mesh" -> do
        mAttributes <- obj .? "mAttributes"
        mPrimitive <- obj .? "mPrimitive"
        pure $ Mesh
          { mAttributes:mAttributes
          , mPrimitive:mPrimitive
          } 
      _ -> Left ("decodeJsonMesh - unknown tag: " <> tag)

