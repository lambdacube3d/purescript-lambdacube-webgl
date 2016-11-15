-- generated file, do not modify!
-- 2016-11-15T20:33:22.535345000000Z

module LambdaCube.IR where
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


type StreamName = Int

type ProgramName = Int

type TextureName = Int

type SamplerName = Int

type UniformName = String

type SlotName = Int

type FrameBufferComponent = Int

type TextureUnit = Int

type RenderTargetName = Int

type TextureUnitMapping = StrMap TextureUnit

data ArrayValue
  = VBoolArray (Array Bool)
  | VIntArray (Array Int32)
  | VWordArray (Array Word32)
  | VFloatArray (Array Float)

data Value
  = VBool Bool
  | VV2B V2B
  | VV3B V3B
  | VV4B V4B
  | VWord Word32
  | VV2U V2U
  | VV3U V3U
  | VV4U V4U
  | VInt Int32
  | VV2I V2I
  | VV3I V3I
  | VV4I V4I
  | VFloat Float
  | VV2F V2F
  | VV3F V3F
  | VV4F V4F
  | VM22F M22F
  | VM23F M23F
  | VM24F M24F
  | VM32F M32F
  | VM33F M33F
  | VM34F M34F
  | VM42F M42F
  | VM43F M43F
  | VM44F M44F

data InputType
  = Bool
  | V2B
  | V3B
  | V4B
  | Word
  | V2U
  | V3U
  | V4U
  | Int
  | V2I
  | V3I
  | V4I
  | Float
  | V2F
  | V3F
  | V4F
  | M22F
  | M23F
  | M24F
  | M32F
  | M33F
  | M34F
  | M42F
  | M43F
  | M44F
  | STexture1D
  | STexture2D
  | STextureCube
  | STexture1DArray
  | STexture2DArray
  | STexture2DRect
  | FTexture1D
  | FTexture2D
  | FTexture3D
  | FTextureCube
  | FTexture1DArray
  | FTexture2DArray
  | FTexture2DMS
  | FTexture2DMSArray
  | FTextureBuffer
  | FTexture2DRect
  | ITexture1D
  | ITexture2D
  | ITexture3D
  | ITextureCube
  | ITexture1DArray
  | ITexture2DArray
  | ITexture2DMS
  | ITexture2DMSArray
  | ITextureBuffer
  | ITexture2DRect
  | UTexture1D
  | UTexture2D
  | UTexture3D
  | UTextureCube
  | UTexture1DArray
  | UTexture2DArray
  | UTexture2DMS
  | UTexture2DMSArray
  | UTextureBuffer
  | UTexture2DRect

data PointSpriteCoordOrigin
  = LowerLeft
  | UpperLeft

data PointSize
  = PointSize Float
  | ProgramPointSize

data PolygonOffset
  = NoOffset
  | Offset Float Float

data FrontFace
  = CCW
  | CW

data PolygonMode
  = PolygonPoint PointSize
  | PolygonLine Float
  | PolygonFill

data ProvokingVertex
  = FirstVertex
  | LastVertex

data CullMode
  = CullNone
  | CullFront FrontFace
  | CullBack FrontFace

data ComparisonFunction
  = Never
  | Less
  | Equal
  | Lequal
  | Greater
  | Notequal
  | Gequal
  | Always

type DepthFunction = ComparisonFunction

data StencilOperation
  = OpZero
  | OpKeep
  | OpReplace
  | OpIncr
  | OpIncrWrap
  | OpDecr
  | OpDecrWrap
  | OpInvert

data BlendEquation
  = FuncAdd
  | FuncSubtract
  | FuncReverseSubtract
  | Min
  | Max

data BlendingFactor
  = Zero
  | One
  | SrcColor
  | OneMinusSrcColor
  | DstColor
  | OneMinusDstColor
  | SrcAlpha
  | OneMinusSrcAlpha
  | DstAlpha
  | OneMinusDstAlpha
  | ConstantColor
  | OneMinusConstantColor
  | ConstantAlpha
  | OneMinusConstantAlpha
  | SrcAlphaSaturate

data LogicOperation
  = Clear
  | And
  | AndReverse
  | Copy
  | AndInverted
  | Noop
  | Xor
  | Or
  | Nor
  | Equiv
  | Invert
  | OrReverse
  | CopyInverted
  | OrInverted
  | Nand
  | Set

data StencilOps
  = StencilOps
  { frontStencilOp :: StencilOperation
  , backStencilOp :: StencilOperation
  }


data StencilTest
  = StencilTest
  { stencilComparision :: ComparisonFunction
  , stencilReference :: Int32
  , stencilMask :: Word32
  }


data StencilTests
  = StencilTests StencilTest StencilTest

data FetchPrimitive
  = Points
  | Lines
  | Triangles
  | LinesAdjacency
  | TrianglesAdjacency

data OutputPrimitive
  = TrianglesOutput
  | LinesOutput
  | PointsOutput

data ColorArity
  = Red
  | RG
  | RGB
  | RGBA

data Blending
  = NoBlending
  | BlendLogicOp LogicOperation
  | Blend
  { colorEqSrc :: BlendEquation
  , alphaEqSrc :: BlendEquation
  , colorFSrc :: BlendingFactor
  , colorFDst :: BlendingFactor
  , alphaFSrc :: BlendingFactor
  , alphaFDst :: BlendingFactor
  , color :: V4F
  }


data RasterContext
  = PointCtx PointSize Float PointSpriteCoordOrigin
  | LineCtx Float ProvokingVertex
  | TriangleCtx CullMode PolygonMode PolygonOffset ProvokingVertex

data FragmentOperation
  = DepthOp DepthFunction Bool
  | StencilOp StencilTests StencilOps StencilOps
  | ColorOp Blending Value

data AccumulationContext
  = AccumulationContext
  { accViewportName :: Maybe String
  , accOperations :: List FragmentOperation
  }


data TextureDataType
  = FloatT ColorArity
  | IntT ColorArity
  | WordT ColorArity
  | ShadowT

data TextureType
  = Texture1D TextureDataType Int
  | Texture2D TextureDataType Int
  | Texture3D TextureDataType
  | TextureCube TextureDataType
  | TextureRect TextureDataType
  | Texture2DMS TextureDataType Int Int Bool
  | TextureBuffer TextureDataType

data MipMap
  = Mip Int Int
  | NoMip
  | AutoMip Int Int

data Filter
  = Nearest
  | Linear
  | NearestMipmapNearest
  | NearestMipmapLinear
  | LinearMipmapNearest
  | LinearMipmapLinear

data EdgeMode
  = Repeat
  | MirroredRepeat
  | ClampToEdge
  | ClampToBorder

data ImageSemantic
  = Depth
  | Stencil
  | Color

data ImageRef
  = TextureImage TextureName Int (Maybe Int)
  | Framebuffer ImageSemantic

data ClearImage
  = ClearImage
  { imageSemantic :: ImageSemantic
  , clearValue :: Value
  }


data Command
  = SetRasterContext RasterContext
  | SetAccumulationContext AccumulationContext
  | SetRenderTarget RenderTargetName
  | SetProgram ProgramName
  | SetSamplerUniform UniformName TextureUnit
  | SetTexture TextureUnit TextureName
  | SetSampler TextureUnit (Maybe SamplerName)
  | RenderSlot SlotName
  | RenderStream StreamName
  | ClearRenderTarget (Array ClearImage)
  | GenerateMipMap TextureUnit
  | SaveImage FrameBufferComponent ImageRef
  | LoadImage ImageRef FrameBufferComponent

data SamplerDescriptor
  = SamplerDescriptor
  { samplerWrapS :: EdgeMode
  , samplerWrapT :: Maybe EdgeMode
  , samplerWrapR :: Maybe EdgeMode
  , samplerMinFilter :: Filter
  , samplerMagFilter :: Filter
  , samplerBorderColor :: Value
  , samplerMinLod :: Maybe Float
  , samplerMaxLod :: Maybe Float
  , samplerLodBias :: Float
  , samplerCompareFunc :: Maybe ComparisonFunction
  }


data TextureDescriptor
  = TextureDescriptor
  { textureType :: TextureType
  , textureSize :: Value
  , textureSemantic :: ImageSemantic
  , textureSampler :: SamplerDescriptor
  , textureBaseLevel :: Int
  , textureMaxLevel :: Int
  }


data Parameter
  = Parameter
  { name :: String
  , ty :: InputType
  }


data Program
  = Program
  { programUniforms :: StrMap InputType
  , programStreams :: StrMap Parameter
  , programInTextures :: StrMap InputType
  , programOutput :: Array Parameter
  , vertexShader :: String
  , geometryShader :: Maybe String
  , fragmentShader :: String
  }


data Slot
  = Slot
  { slotName :: String
  , slotStreams :: StrMap InputType
  , slotUniforms :: StrMap InputType
  , slotPrimitive :: FetchPrimitive
  , slotPrograms :: Array ProgramName
  }


data StreamData
  = StreamData
  { streamData :: StrMap ArrayValue
  , streamType :: StrMap InputType
  , streamPrimitive :: FetchPrimitive
  , streamPrograms :: Array ProgramName
  }


data TargetItem
  = TargetItem
  { targetSemantic :: ImageSemantic
  , targetRef :: Maybe ImageRef
  }


data RenderTarget
  = RenderTarget
  { renderTargets :: Array TargetItem
  }


data Backend
  = WebGL1
  | OpenGL33

data Pipeline
  = Pipeline
  { info :: String
  , backend :: Backend
  , textures :: Array TextureDescriptor
  , samplers :: Array SamplerDescriptor
  , targets :: Array RenderTarget
  , programs :: Array Program
  , slots :: Array Slot
  , streams :: Array StreamData
  , commands :: Array Command
  }



derive instance genericFetchPrimitive :: Generic FetchPrimitive
instance showFetchPrimitive :: Show FetchPrimitive where show = gShow
instance eqFetchPrimitive   :: Eq FetchPrimitive   where eq = gEq

derive instance genericColorArity :: Generic ColorArity
instance showColorArity :: Show ColorArity where show = gShow

derive instance genericTextureDataType :: Generic TextureDataType
instance showTextureDataType :: Show TextureDataType where show = gShow

instance encodeJsonArrayValue :: EncodeJson ArrayValue where
  encodeJson v = case v of
    VBoolArray arg0 -> "tag" := "VBoolArray" ~> "arg0" := arg0 ~> jsonEmptyObject
    VIntArray arg0 -> "tag" := "VIntArray" ~> "arg0" := arg0 ~> jsonEmptyObject
    VWordArray arg0 -> "tag" := "VWordArray" ~> "arg0" := arg0 ~> jsonEmptyObject
    VFloatArray arg0 -> "tag" := "VFloatArray" ~> "arg0" := arg0 ~> jsonEmptyObject

instance decodeJsonArrayValue :: DecodeJson ArrayValue where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "VBoolArray" -> VBoolArray <$> obj .? "arg0"
      "VIntArray" -> VIntArray <$> obj .? "arg0"
      "VWordArray" -> VWordArray <$> obj .? "arg0"
      "VFloatArray" -> VFloatArray <$> obj .? "arg0"
      _ -> Left ("decodeJsonArrayValue - unknown tag: " <> tag)

instance encodeJsonValue :: EncodeJson Value where
  encodeJson v = case v of
    VBool arg0 -> "tag" := "VBool" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV2B arg0 -> "tag" := "VV2B" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV3B arg0 -> "tag" := "VV3B" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV4B arg0 -> "tag" := "VV4B" ~> "arg0" := arg0 ~> jsonEmptyObject
    VWord arg0 -> "tag" := "VWord" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV2U arg0 -> "tag" := "VV2U" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV3U arg0 -> "tag" := "VV3U" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV4U arg0 -> "tag" := "VV4U" ~> "arg0" := arg0 ~> jsonEmptyObject
    VInt arg0 -> "tag" := "VInt" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV2I arg0 -> "tag" := "VV2I" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV3I arg0 -> "tag" := "VV3I" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV4I arg0 -> "tag" := "VV4I" ~> "arg0" := arg0 ~> jsonEmptyObject
    VFloat arg0 -> "tag" := "VFloat" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV2F arg0 -> "tag" := "VV2F" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV3F arg0 -> "tag" := "VV3F" ~> "arg0" := arg0 ~> jsonEmptyObject
    VV4F arg0 -> "tag" := "VV4F" ~> "arg0" := arg0 ~> jsonEmptyObject
    VM22F arg0 -> "tag" := "VM22F" ~> "arg0" := arg0 ~> jsonEmptyObject
    VM23F arg0 -> "tag" := "VM23F" ~> "arg0" := arg0 ~> jsonEmptyObject
    VM24F arg0 -> "tag" := "VM24F" ~> "arg0" := arg0 ~> jsonEmptyObject
    VM32F arg0 -> "tag" := "VM32F" ~> "arg0" := arg0 ~> jsonEmptyObject
    VM33F arg0 -> "tag" := "VM33F" ~> "arg0" := arg0 ~> jsonEmptyObject
    VM34F arg0 -> "tag" := "VM34F" ~> "arg0" := arg0 ~> jsonEmptyObject
    VM42F arg0 -> "tag" := "VM42F" ~> "arg0" := arg0 ~> jsonEmptyObject
    VM43F arg0 -> "tag" := "VM43F" ~> "arg0" := arg0 ~> jsonEmptyObject
    VM44F arg0 -> "tag" := "VM44F" ~> "arg0" := arg0 ~> jsonEmptyObject

instance decodeJsonValue :: DecodeJson Value where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "VBool" -> VBool <$> obj .? "arg0"
      "VV2B" -> VV2B <$> obj .? "arg0"
      "VV3B" -> VV3B <$> obj .? "arg0"
      "VV4B" -> VV4B <$> obj .? "arg0"
      "VWord" -> VWord <$> obj .? "arg0"
      "VV2U" -> VV2U <$> obj .? "arg0"
      "VV3U" -> VV3U <$> obj .? "arg0"
      "VV4U" -> VV4U <$> obj .? "arg0"
      "VInt" -> VInt <$> obj .? "arg0"
      "VV2I" -> VV2I <$> obj .? "arg0"
      "VV3I" -> VV3I <$> obj .? "arg0"
      "VV4I" -> VV4I <$> obj .? "arg0"
      "VFloat" -> VFloat <$> obj .? "arg0"
      "VV2F" -> VV2F <$> obj .? "arg0"
      "VV3F" -> VV3F <$> obj .? "arg0"
      "VV4F" -> VV4F <$> obj .? "arg0"
      "VM22F" -> VM22F <$> obj .? "arg0"
      "VM23F" -> VM23F <$> obj .? "arg0"
      "VM24F" -> VM24F <$> obj .? "arg0"
      "VM32F" -> VM32F <$> obj .? "arg0"
      "VM33F" -> VM33F <$> obj .? "arg0"
      "VM34F" -> VM34F <$> obj .? "arg0"
      "VM42F" -> VM42F <$> obj .? "arg0"
      "VM43F" -> VM43F <$> obj .? "arg0"
      "VM44F" -> VM44F <$> obj .? "arg0"
      _ -> Left ("decodeJsonValue - unknown tag: " <> tag)

instance encodeJsonInputType :: EncodeJson InputType where
  encodeJson v = case v of
    Bool -> "tag" := "Bool" ~> jsonEmptyObject
    V2B -> "tag" := "V2B" ~> jsonEmptyObject
    V3B -> "tag" := "V3B" ~> jsonEmptyObject
    V4B -> "tag" := "V4B" ~> jsonEmptyObject
    Word -> "tag" := "Word" ~> jsonEmptyObject
    V2U -> "tag" := "V2U" ~> jsonEmptyObject
    V3U -> "tag" := "V3U" ~> jsonEmptyObject
    V4U -> "tag" := "V4U" ~> jsonEmptyObject
    Int -> "tag" := "Int" ~> jsonEmptyObject
    V2I -> "tag" := "V2I" ~> jsonEmptyObject
    V3I -> "tag" := "V3I" ~> jsonEmptyObject
    V4I -> "tag" := "V4I" ~> jsonEmptyObject
    Float -> "tag" := "Float" ~> jsonEmptyObject
    V2F -> "tag" := "V2F" ~> jsonEmptyObject
    V3F -> "tag" := "V3F" ~> jsonEmptyObject
    V4F -> "tag" := "V4F" ~> jsonEmptyObject
    M22F -> "tag" := "M22F" ~> jsonEmptyObject
    M23F -> "tag" := "M23F" ~> jsonEmptyObject
    M24F -> "tag" := "M24F" ~> jsonEmptyObject
    M32F -> "tag" := "M32F" ~> jsonEmptyObject
    M33F -> "tag" := "M33F" ~> jsonEmptyObject
    M34F -> "tag" := "M34F" ~> jsonEmptyObject
    M42F -> "tag" := "M42F" ~> jsonEmptyObject
    M43F -> "tag" := "M43F" ~> jsonEmptyObject
    M44F -> "tag" := "M44F" ~> jsonEmptyObject
    STexture1D -> "tag" := "STexture1D" ~> jsonEmptyObject
    STexture2D -> "tag" := "STexture2D" ~> jsonEmptyObject
    STextureCube -> "tag" := "STextureCube" ~> jsonEmptyObject
    STexture1DArray -> "tag" := "STexture1DArray" ~> jsonEmptyObject
    STexture2DArray -> "tag" := "STexture2DArray" ~> jsonEmptyObject
    STexture2DRect -> "tag" := "STexture2DRect" ~> jsonEmptyObject
    FTexture1D -> "tag" := "FTexture1D" ~> jsonEmptyObject
    FTexture2D -> "tag" := "FTexture2D" ~> jsonEmptyObject
    FTexture3D -> "tag" := "FTexture3D" ~> jsonEmptyObject
    FTextureCube -> "tag" := "FTextureCube" ~> jsonEmptyObject
    FTexture1DArray -> "tag" := "FTexture1DArray" ~> jsonEmptyObject
    FTexture2DArray -> "tag" := "FTexture2DArray" ~> jsonEmptyObject
    FTexture2DMS -> "tag" := "FTexture2DMS" ~> jsonEmptyObject
    FTexture2DMSArray -> "tag" := "FTexture2DMSArray" ~> jsonEmptyObject
    FTextureBuffer -> "tag" := "FTextureBuffer" ~> jsonEmptyObject
    FTexture2DRect -> "tag" := "FTexture2DRect" ~> jsonEmptyObject
    ITexture1D -> "tag" := "ITexture1D" ~> jsonEmptyObject
    ITexture2D -> "tag" := "ITexture2D" ~> jsonEmptyObject
    ITexture3D -> "tag" := "ITexture3D" ~> jsonEmptyObject
    ITextureCube -> "tag" := "ITextureCube" ~> jsonEmptyObject
    ITexture1DArray -> "tag" := "ITexture1DArray" ~> jsonEmptyObject
    ITexture2DArray -> "tag" := "ITexture2DArray" ~> jsonEmptyObject
    ITexture2DMS -> "tag" := "ITexture2DMS" ~> jsonEmptyObject
    ITexture2DMSArray -> "tag" := "ITexture2DMSArray" ~> jsonEmptyObject
    ITextureBuffer -> "tag" := "ITextureBuffer" ~> jsonEmptyObject
    ITexture2DRect -> "tag" := "ITexture2DRect" ~> jsonEmptyObject
    UTexture1D -> "tag" := "UTexture1D" ~> jsonEmptyObject
    UTexture2D -> "tag" := "UTexture2D" ~> jsonEmptyObject
    UTexture3D -> "tag" := "UTexture3D" ~> jsonEmptyObject
    UTextureCube -> "tag" := "UTextureCube" ~> jsonEmptyObject
    UTexture1DArray -> "tag" := "UTexture1DArray" ~> jsonEmptyObject
    UTexture2DArray -> "tag" := "UTexture2DArray" ~> jsonEmptyObject
    UTexture2DMS -> "tag" := "UTexture2DMS" ~> jsonEmptyObject
    UTexture2DMSArray -> "tag" := "UTexture2DMSArray" ~> jsonEmptyObject
    UTextureBuffer -> "tag" := "UTextureBuffer" ~> jsonEmptyObject
    UTexture2DRect -> "tag" := "UTexture2DRect" ~> jsonEmptyObject

instance decodeJsonInputType :: DecodeJson InputType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Bool" -> pure Bool
      "V2B" -> pure V2B
      "V3B" -> pure V3B
      "V4B" -> pure V4B
      "Word" -> pure Word
      "V2U" -> pure V2U
      "V3U" -> pure V3U
      "V4U" -> pure V4U
      "Int" -> pure Int
      "V2I" -> pure V2I
      "V3I" -> pure V3I
      "V4I" -> pure V4I
      "Float" -> pure Float
      "V2F" -> pure V2F
      "V3F" -> pure V3F
      "V4F" -> pure V4F
      "M22F" -> pure M22F
      "M23F" -> pure M23F
      "M24F" -> pure M24F
      "M32F" -> pure M32F
      "M33F" -> pure M33F
      "M34F" -> pure M34F
      "M42F" -> pure M42F
      "M43F" -> pure M43F
      "M44F" -> pure M44F
      "STexture1D" -> pure STexture1D
      "STexture2D" -> pure STexture2D
      "STextureCube" -> pure STextureCube
      "STexture1DArray" -> pure STexture1DArray
      "STexture2DArray" -> pure STexture2DArray
      "STexture2DRect" -> pure STexture2DRect
      "FTexture1D" -> pure FTexture1D
      "FTexture2D" -> pure FTexture2D
      "FTexture3D" -> pure FTexture3D
      "FTextureCube" -> pure FTextureCube
      "FTexture1DArray" -> pure FTexture1DArray
      "FTexture2DArray" -> pure FTexture2DArray
      "FTexture2DMS" -> pure FTexture2DMS
      "FTexture2DMSArray" -> pure FTexture2DMSArray
      "FTextureBuffer" -> pure FTextureBuffer
      "FTexture2DRect" -> pure FTexture2DRect
      "ITexture1D" -> pure ITexture1D
      "ITexture2D" -> pure ITexture2D
      "ITexture3D" -> pure ITexture3D
      "ITextureCube" -> pure ITextureCube
      "ITexture1DArray" -> pure ITexture1DArray
      "ITexture2DArray" -> pure ITexture2DArray
      "ITexture2DMS" -> pure ITexture2DMS
      "ITexture2DMSArray" -> pure ITexture2DMSArray
      "ITextureBuffer" -> pure ITextureBuffer
      "ITexture2DRect" -> pure ITexture2DRect
      "UTexture1D" -> pure UTexture1D
      "UTexture2D" -> pure UTexture2D
      "UTexture3D" -> pure UTexture3D
      "UTextureCube" -> pure UTextureCube
      "UTexture1DArray" -> pure UTexture1DArray
      "UTexture2DArray" -> pure UTexture2DArray
      "UTexture2DMS" -> pure UTexture2DMS
      "UTexture2DMSArray" -> pure UTexture2DMSArray
      "UTextureBuffer" -> pure UTextureBuffer
      "UTexture2DRect" -> pure UTexture2DRect
      _ -> Left ("decodeJsonInputType - unknown tag: " <> tag)

instance encodeJsonPointSpriteCoordOrigin :: EncodeJson PointSpriteCoordOrigin where
  encodeJson v = case v of
    LowerLeft -> "tag" := "LowerLeft" ~> jsonEmptyObject
    UpperLeft -> "tag" := "UpperLeft" ~> jsonEmptyObject

instance decodeJsonPointSpriteCoordOrigin :: DecodeJson PointSpriteCoordOrigin where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "LowerLeft" -> pure LowerLeft
      "UpperLeft" -> pure UpperLeft
      _ -> Left ("decodeJsonPointSpriteCoordOrigin - unknown tag: " <> tag)

instance encodeJsonPointSize :: EncodeJson PointSize where
  encodeJson v = case v of
    PointSize arg0 -> "tag" := "PointSize" ~> "arg0" := arg0 ~> jsonEmptyObject
    ProgramPointSize -> "tag" := "ProgramPointSize" ~> jsonEmptyObject

instance decodeJsonPointSize :: DecodeJson PointSize where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "PointSize" -> PointSize <$> obj .? "arg0"
      "ProgramPointSize" -> pure ProgramPointSize
      _ -> Left ("decodeJsonPointSize - unknown tag: " <> tag)

instance encodeJsonPolygonOffset :: EncodeJson PolygonOffset where
  encodeJson v = case v of
    NoOffset -> "tag" := "NoOffset" ~> jsonEmptyObject
    Offset arg0 arg1 -> "tag" := "Offset" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject

instance decodeJsonPolygonOffset :: DecodeJson PolygonOffset where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "NoOffset" -> pure NoOffset
      "Offset" -> Offset <$> obj .? "arg0" <*> obj .? "arg1"
      _ -> Left ("decodeJsonPolygonOffset - unknown tag: " <> tag)

instance encodeJsonFrontFace :: EncodeJson FrontFace where
  encodeJson v = case v of
    CCW -> "tag" := "CCW" ~> jsonEmptyObject
    CW -> "tag" := "CW" ~> jsonEmptyObject

instance decodeJsonFrontFace :: DecodeJson FrontFace where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "CCW" -> pure CCW
      "CW" -> pure CW
      _ -> Left ("decodeJsonFrontFace - unknown tag: " <> tag)

instance encodeJsonPolygonMode :: EncodeJson PolygonMode where
  encodeJson v = case v of
    PolygonPoint arg0 -> "tag" := "PolygonPoint" ~> "arg0" := arg0 ~> jsonEmptyObject
    PolygonLine arg0 -> "tag" := "PolygonLine" ~> "arg0" := arg0 ~> jsonEmptyObject
    PolygonFill -> "tag" := "PolygonFill" ~> jsonEmptyObject

instance decodeJsonPolygonMode :: DecodeJson PolygonMode where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "PolygonPoint" -> PolygonPoint <$> obj .? "arg0"
      "PolygonLine" -> PolygonLine <$> obj .? "arg0"
      "PolygonFill" -> pure PolygonFill
      _ -> Left ("decodeJsonPolygonMode - unknown tag: " <> tag)

instance encodeJsonProvokingVertex :: EncodeJson ProvokingVertex where
  encodeJson v = case v of
    FirstVertex -> "tag" := "FirstVertex" ~> jsonEmptyObject
    LastVertex -> "tag" := "LastVertex" ~> jsonEmptyObject

instance decodeJsonProvokingVertex :: DecodeJson ProvokingVertex where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "FirstVertex" -> pure FirstVertex
      "LastVertex" -> pure LastVertex
      _ -> Left ("decodeJsonProvokingVertex - unknown tag: " <> tag)

instance encodeJsonCullMode :: EncodeJson CullMode where
  encodeJson v = case v of
    CullNone -> "tag" := "CullNone" ~> jsonEmptyObject
    CullFront arg0 -> "tag" := "CullFront" ~> "arg0" := arg0 ~> jsonEmptyObject
    CullBack arg0 -> "tag" := "CullBack" ~> "arg0" := arg0 ~> jsonEmptyObject

instance decodeJsonCullMode :: DecodeJson CullMode where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "CullNone" -> pure CullNone
      "CullFront" -> CullFront <$> obj .? "arg0"
      "CullBack" -> CullBack <$> obj .? "arg0"
      _ -> Left ("decodeJsonCullMode - unknown tag: " <> tag)

instance encodeJsonComparisonFunction :: EncodeJson ComparisonFunction where
  encodeJson v = case v of
    Never -> "tag" := "Never" ~> jsonEmptyObject
    Less -> "tag" := "Less" ~> jsonEmptyObject
    Equal -> "tag" := "Equal" ~> jsonEmptyObject
    Lequal -> "tag" := "Lequal" ~> jsonEmptyObject
    Greater -> "tag" := "Greater" ~> jsonEmptyObject
    Notequal -> "tag" := "Notequal" ~> jsonEmptyObject
    Gequal -> "tag" := "Gequal" ~> jsonEmptyObject
    Always -> "tag" := "Always" ~> jsonEmptyObject

instance decodeJsonComparisonFunction :: DecodeJson ComparisonFunction where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Never" -> pure Never
      "Less" -> pure Less
      "Equal" -> pure Equal
      "Lequal" -> pure Lequal
      "Greater" -> pure Greater
      "Notequal" -> pure Notequal
      "Gequal" -> pure Gequal
      "Always" -> pure Always
      _ -> Left ("decodeJsonComparisonFunction - unknown tag: " <> tag)

instance encodeJsonStencilOperation :: EncodeJson StencilOperation where
  encodeJson v = case v of
    OpZero -> "tag" := "OpZero" ~> jsonEmptyObject
    OpKeep -> "tag" := "OpKeep" ~> jsonEmptyObject
    OpReplace -> "tag" := "OpReplace" ~> jsonEmptyObject
    OpIncr -> "tag" := "OpIncr" ~> jsonEmptyObject
    OpIncrWrap -> "tag" := "OpIncrWrap" ~> jsonEmptyObject
    OpDecr -> "tag" := "OpDecr" ~> jsonEmptyObject
    OpDecrWrap -> "tag" := "OpDecrWrap" ~> jsonEmptyObject
    OpInvert -> "tag" := "OpInvert" ~> jsonEmptyObject

instance decodeJsonStencilOperation :: DecodeJson StencilOperation where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "OpZero" -> pure OpZero
      "OpKeep" -> pure OpKeep
      "OpReplace" -> pure OpReplace
      "OpIncr" -> pure OpIncr
      "OpIncrWrap" -> pure OpIncrWrap
      "OpDecr" -> pure OpDecr
      "OpDecrWrap" -> pure OpDecrWrap
      "OpInvert" -> pure OpInvert
      _ -> Left ("decodeJsonStencilOperation - unknown tag: " <> tag)

instance encodeJsonBlendEquation :: EncodeJson BlendEquation where
  encodeJson v = case v of
    FuncAdd -> "tag" := "FuncAdd" ~> jsonEmptyObject
    FuncSubtract -> "tag" := "FuncSubtract" ~> jsonEmptyObject
    FuncReverseSubtract -> "tag" := "FuncReverseSubtract" ~> jsonEmptyObject
    Min -> "tag" := "Min" ~> jsonEmptyObject
    Max -> "tag" := "Max" ~> jsonEmptyObject

instance decodeJsonBlendEquation :: DecodeJson BlendEquation where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "FuncAdd" -> pure FuncAdd
      "FuncSubtract" -> pure FuncSubtract
      "FuncReverseSubtract" -> pure FuncReverseSubtract
      "Min" -> pure Min
      "Max" -> pure Max
      _ -> Left ("decodeJsonBlendEquation - unknown tag: " <> tag)

instance encodeJsonBlendingFactor :: EncodeJson BlendingFactor where
  encodeJson v = case v of
    Zero -> "tag" := "Zero" ~> jsonEmptyObject
    One -> "tag" := "One" ~> jsonEmptyObject
    SrcColor -> "tag" := "SrcColor" ~> jsonEmptyObject
    OneMinusSrcColor -> "tag" := "OneMinusSrcColor" ~> jsonEmptyObject
    DstColor -> "tag" := "DstColor" ~> jsonEmptyObject
    OneMinusDstColor -> "tag" := "OneMinusDstColor" ~> jsonEmptyObject
    SrcAlpha -> "tag" := "SrcAlpha" ~> jsonEmptyObject
    OneMinusSrcAlpha -> "tag" := "OneMinusSrcAlpha" ~> jsonEmptyObject
    DstAlpha -> "tag" := "DstAlpha" ~> jsonEmptyObject
    OneMinusDstAlpha -> "tag" := "OneMinusDstAlpha" ~> jsonEmptyObject
    ConstantColor -> "tag" := "ConstantColor" ~> jsonEmptyObject
    OneMinusConstantColor -> "tag" := "OneMinusConstantColor" ~> jsonEmptyObject
    ConstantAlpha -> "tag" := "ConstantAlpha" ~> jsonEmptyObject
    OneMinusConstantAlpha -> "tag" := "OneMinusConstantAlpha" ~> jsonEmptyObject
    SrcAlphaSaturate -> "tag" := "SrcAlphaSaturate" ~> jsonEmptyObject

instance decodeJsonBlendingFactor :: DecodeJson BlendingFactor where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Zero" -> pure Zero
      "One" -> pure One
      "SrcColor" -> pure SrcColor
      "OneMinusSrcColor" -> pure OneMinusSrcColor
      "DstColor" -> pure DstColor
      "OneMinusDstColor" -> pure OneMinusDstColor
      "SrcAlpha" -> pure SrcAlpha
      "OneMinusSrcAlpha" -> pure OneMinusSrcAlpha
      "DstAlpha" -> pure DstAlpha
      "OneMinusDstAlpha" -> pure OneMinusDstAlpha
      "ConstantColor" -> pure ConstantColor
      "OneMinusConstantColor" -> pure OneMinusConstantColor
      "ConstantAlpha" -> pure ConstantAlpha
      "OneMinusConstantAlpha" -> pure OneMinusConstantAlpha
      "SrcAlphaSaturate" -> pure SrcAlphaSaturate
      _ -> Left ("decodeJsonBlendingFactor - unknown tag: " <> tag)

instance encodeJsonLogicOperation :: EncodeJson LogicOperation where
  encodeJson v = case v of
    Clear -> "tag" := "Clear" ~> jsonEmptyObject
    And -> "tag" := "And" ~> jsonEmptyObject
    AndReverse -> "tag" := "AndReverse" ~> jsonEmptyObject
    Copy -> "tag" := "Copy" ~> jsonEmptyObject
    AndInverted -> "tag" := "AndInverted" ~> jsonEmptyObject
    Noop -> "tag" := "Noop" ~> jsonEmptyObject
    Xor -> "tag" := "Xor" ~> jsonEmptyObject
    Or -> "tag" := "Or" ~> jsonEmptyObject
    Nor -> "tag" := "Nor" ~> jsonEmptyObject
    Equiv -> "tag" := "Equiv" ~> jsonEmptyObject
    Invert -> "tag" := "Invert" ~> jsonEmptyObject
    OrReverse -> "tag" := "OrReverse" ~> jsonEmptyObject
    CopyInverted -> "tag" := "CopyInverted" ~> jsonEmptyObject
    OrInverted -> "tag" := "OrInverted" ~> jsonEmptyObject
    Nand -> "tag" := "Nand" ~> jsonEmptyObject
    Set -> "tag" := "Set" ~> jsonEmptyObject

instance decodeJsonLogicOperation :: DecodeJson LogicOperation where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Clear" -> pure Clear
      "And" -> pure And
      "AndReverse" -> pure AndReverse
      "Copy" -> pure Copy
      "AndInverted" -> pure AndInverted
      "Noop" -> pure Noop
      "Xor" -> pure Xor
      "Or" -> pure Or
      "Nor" -> pure Nor
      "Equiv" -> pure Equiv
      "Invert" -> pure Invert
      "OrReverse" -> pure OrReverse
      "CopyInverted" -> pure CopyInverted
      "OrInverted" -> pure OrInverted
      "Nand" -> pure Nand
      "Set" -> pure Set
      _ -> Left ("decodeJsonLogicOperation - unknown tag: " <> tag)

instance encodeJsonStencilOps :: EncodeJson StencilOps where
  encodeJson v = case v of
    StencilOps r ->
      "tag" := "StencilOps" ~>
      "frontStencilOp" := r.frontStencilOp ~>
      "backStencilOp" := r.backStencilOp ~>
      jsonEmptyObject

instance decodeJsonStencilOps :: DecodeJson StencilOps where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "StencilOps" -> do
        frontStencilOp <- obj .? "frontStencilOp"
        backStencilOp <- obj .? "backStencilOp"
        pure $ StencilOps
          { frontStencilOp:frontStencilOp
          , backStencilOp:backStencilOp
          } 
      _ -> Left ("decodeJsonStencilOps - unknown tag: " <> tag)

instance encodeJsonStencilTest :: EncodeJson StencilTest where
  encodeJson v = case v of
    StencilTest r ->
      "tag" := "StencilTest" ~>
      "stencilComparision" := r.stencilComparision ~>
      "stencilReference" := r.stencilReference ~>
      "stencilMask" := r.stencilMask ~>
      jsonEmptyObject

instance decodeJsonStencilTest :: DecodeJson StencilTest where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "StencilTest" -> do
        stencilComparision <- obj .? "stencilComparision"
        stencilReference <- obj .? "stencilReference"
        stencilMask <- obj .? "stencilMask"
        pure $ StencilTest
          { stencilComparision:stencilComparision
          , stencilReference:stencilReference
          , stencilMask:stencilMask
          } 
      _ -> Left ("decodeJsonStencilTest - unknown tag: " <> tag)

instance encodeJsonStencilTests :: EncodeJson StencilTests where
  encodeJson v = case v of
    StencilTests arg0 arg1 -> "tag" := "StencilTests" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject

instance decodeJsonStencilTests :: DecodeJson StencilTests where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "StencilTests" -> StencilTests <$> obj .? "arg0" <*> obj .? "arg1"
      _ -> Left ("decodeJsonStencilTests - unknown tag: " <> tag)

instance encodeJsonFetchPrimitive :: EncodeJson FetchPrimitive where
  encodeJson v = case v of
    Points -> "tag" := "Points" ~> jsonEmptyObject
    Lines -> "tag" := "Lines" ~> jsonEmptyObject
    Triangles -> "tag" := "Triangles" ~> jsonEmptyObject
    LinesAdjacency -> "tag" := "LinesAdjacency" ~> jsonEmptyObject
    TrianglesAdjacency -> "tag" := "TrianglesAdjacency" ~> jsonEmptyObject

instance decodeJsonFetchPrimitive :: DecodeJson FetchPrimitive where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Points" -> pure Points
      "Lines" -> pure Lines
      "Triangles" -> pure Triangles
      "LinesAdjacency" -> pure LinesAdjacency
      "TrianglesAdjacency" -> pure TrianglesAdjacency
      _ -> Left ("decodeJsonFetchPrimitive - unknown tag: " <> tag)

instance encodeJsonOutputPrimitive :: EncodeJson OutputPrimitive where
  encodeJson v = case v of
    TrianglesOutput -> "tag" := "TrianglesOutput" ~> jsonEmptyObject
    LinesOutput -> "tag" := "LinesOutput" ~> jsonEmptyObject
    PointsOutput -> "tag" := "PointsOutput" ~> jsonEmptyObject

instance decodeJsonOutputPrimitive :: DecodeJson OutputPrimitive where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TrianglesOutput" -> pure TrianglesOutput
      "LinesOutput" -> pure LinesOutput
      "PointsOutput" -> pure PointsOutput
      _ -> Left ("decodeJsonOutputPrimitive - unknown tag: " <> tag)

instance encodeJsonColorArity :: EncodeJson ColorArity where
  encodeJson v = case v of
    Red -> "tag" := "Red" ~> jsonEmptyObject
    RG -> "tag" := "RG" ~> jsonEmptyObject
    RGB -> "tag" := "RGB" ~> jsonEmptyObject
    RGBA -> "tag" := "RGBA" ~> jsonEmptyObject

instance decodeJsonColorArity :: DecodeJson ColorArity where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Red" -> pure Red
      "RG" -> pure RG
      "RGB" -> pure RGB
      "RGBA" -> pure RGBA
      _ -> Left ("decodeJsonColorArity - unknown tag: " <> tag)

instance encodeJsonBlending :: EncodeJson Blending where
  encodeJson v = case v of
    NoBlending -> "tag" := "NoBlending" ~> jsonEmptyObject
    BlendLogicOp arg0 -> "tag" := "BlendLogicOp" ~> "arg0" := arg0 ~> jsonEmptyObject
    Blend r ->
      "tag" := "Blend" ~>
      "colorEqSrc" := r.colorEqSrc ~>
      "alphaEqSrc" := r.alphaEqSrc ~>
      "colorFSrc" := r.colorFSrc ~>
      "colorFDst" := r.colorFDst ~>
      "alphaFSrc" := r.alphaFSrc ~>
      "alphaFDst" := r.alphaFDst ~>
      "color" := r.color ~>
      jsonEmptyObject

instance decodeJsonBlending :: DecodeJson Blending where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "NoBlending" -> pure NoBlending
      "BlendLogicOp" -> BlendLogicOp <$> obj .? "arg0"
      "Blend" -> do
        colorEqSrc <- obj .? "colorEqSrc"
        alphaEqSrc <- obj .? "alphaEqSrc"
        colorFSrc <- obj .? "colorFSrc"
        colorFDst <- obj .? "colorFDst"
        alphaFSrc <- obj .? "alphaFSrc"
        alphaFDst <- obj .? "alphaFDst"
        color <- obj .? "color"
        pure $ Blend
          { colorEqSrc:colorEqSrc
          , alphaEqSrc:alphaEqSrc
          , colorFSrc:colorFSrc
          , colorFDst:colorFDst
          , alphaFSrc:alphaFSrc
          , alphaFDst:alphaFDst
          , color:color
          } 
      _ -> Left ("decodeJsonBlending - unknown tag: " <> tag)

instance encodeJsonRasterContext :: EncodeJson RasterContext where
  encodeJson v = case v of
    PointCtx arg0 arg1 arg2 -> "tag" := "PointCtx" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> "arg2" := arg2 ~> jsonEmptyObject
    LineCtx arg0 arg1 -> "tag" := "LineCtx" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject
    TriangleCtx arg0 arg1 arg2 arg3 -> "tag" := "TriangleCtx" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> "arg2" := arg2 ~> "arg3" := arg3 ~> jsonEmptyObject

instance decodeJsonRasterContext :: DecodeJson RasterContext where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "PointCtx" -> PointCtx <$> obj .? "arg0" <*> obj .? "arg1" <*> obj .? "arg2"
      "LineCtx" -> LineCtx <$> obj .? "arg0" <*> obj .? "arg1"
      "TriangleCtx" -> TriangleCtx <$> obj .? "arg0" <*> obj .? "arg1" <*> obj .? "arg2" <*> obj .? "arg3"
      _ -> Left ("decodeJsonRasterContext - unknown tag: " <> tag)

instance encodeJsonFragmentOperation :: EncodeJson FragmentOperation where
  encodeJson v = case v of
    DepthOp arg0 arg1 -> "tag" := "DepthOp" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject
    StencilOp arg0 arg1 arg2 -> "tag" := "StencilOp" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> "arg2" := arg2 ~> jsonEmptyObject
    ColorOp arg0 arg1 -> "tag" := "ColorOp" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject

instance decodeJsonFragmentOperation :: DecodeJson FragmentOperation where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "DepthOp" -> DepthOp <$> obj .? "arg0" <*> obj .? "arg1"
      "StencilOp" -> StencilOp <$> obj .? "arg0" <*> obj .? "arg1" <*> obj .? "arg2"
      "ColorOp" -> ColorOp <$> obj .? "arg0" <*> obj .? "arg1"
      _ -> Left ("decodeJsonFragmentOperation - unknown tag: " <> tag)

instance encodeJsonAccumulationContext :: EncodeJson AccumulationContext where
  encodeJson v = case v of
    AccumulationContext r ->
      "tag" := "AccumulationContext" ~>
      "accViewportName" := r.accViewportName ~>
      "accOperations" := r.accOperations ~>
      jsonEmptyObject

instance decodeJsonAccumulationContext :: DecodeJson AccumulationContext where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "AccumulationContext" -> do
        accViewportName <- obj .? "accViewportName"
        accOperations <- obj .? "accOperations"
        pure $ AccumulationContext
          { accViewportName:accViewportName
          , accOperations:accOperations
          } 
      _ -> Left ("decodeJsonAccumulationContext - unknown tag: " <> tag)

instance encodeJsonTextureDataType :: EncodeJson TextureDataType where
  encodeJson v = case v of
    FloatT arg0 -> "tag" := "FloatT" ~> "arg0" := arg0 ~> jsonEmptyObject
    IntT arg0 -> "tag" := "IntT" ~> "arg0" := arg0 ~> jsonEmptyObject
    WordT arg0 -> "tag" := "WordT" ~> "arg0" := arg0 ~> jsonEmptyObject
    ShadowT -> "tag" := "ShadowT" ~> jsonEmptyObject

instance decodeJsonTextureDataType :: DecodeJson TextureDataType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "FloatT" -> FloatT <$> obj .? "arg0"
      "IntT" -> IntT <$> obj .? "arg0"
      "WordT" -> WordT <$> obj .? "arg0"
      "ShadowT" -> pure ShadowT
      _ -> Left ("decodeJsonTextureDataType - unknown tag: " <> tag)

instance encodeJsonTextureType :: EncodeJson TextureType where
  encodeJson v = case v of
    Texture1D arg0 arg1 -> "tag" := "Texture1D" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject
    Texture2D arg0 arg1 -> "tag" := "Texture2D" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject
    Texture3D arg0 -> "tag" := "Texture3D" ~> "arg0" := arg0 ~> jsonEmptyObject
    TextureCube arg0 -> "tag" := "TextureCube" ~> "arg0" := arg0 ~> jsonEmptyObject
    TextureRect arg0 -> "tag" := "TextureRect" ~> "arg0" := arg0 ~> jsonEmptyObject
    Texture2DMS arg0 arg1 arg2 arg3 -> "tag" := "Texture2DMS" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> "arg2" := arg2 ~> "arg3" := arg3 ~> jsonEmptyObject
    TextureBuffer arg0 -> "tag" := "TextureBuffer" ~> "arg0" := arg0 ~> jsonEmptyObject

instance decodeJsonTextureType :: DecodeJson TextureType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Texture1D" -> Texture1D <$> obj .? "arg0" <*> obj .? "arg1"
      "Texture2D" -> Texture2D <$> obj .? "arg0" <*> obj .? "arg1"
      "Texture3D" -> Texture3D <$> obj .? "arg0"
      "TextureCube" -> TextureCube <$> obj .? "arg0"
      "TextureRect" -> TextureRect <$> obj .? "arg0"
      "Texture2DMS" -> Texture2DMS <$> obj .? "arg0" <*> obj .? "arg1" <*> obj .? "arg2" <*> obj .? "arg3"
      "TextureBuffer" -> TextureBuffer <$> obj .? "arg0"
      _ -> Left ("decodeJsonTextureType - unknown tag: " <> tag)

instance encodeJsonMipMap :: EncodeJson MipMap where
  encodeJson v = case v of
    Mip arg0 arg1 -> "tag" := "Mip" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject
    NoMip -> "tag" := "NoMip" ~> jsonEmptyObject
    AutoMip arg0 arg1 -> "tag" := "AutoMip" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject

instance decodeJsonMipMap :: DecodeJson MipMap where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Mip" -> Mip <$> obj .? "arg0" <*> obj .? "arg1"
      "NoMip" -> pure NoMip
      "AutoMip" -> AutoMip <$> obj .? "arg0" <*> obj .? "arg1"
      _ -> Left ("decodeJsonMipMap - unknown tag: " <> tag)

instance encodeJsonFilter :: EncodeJson Filter where
  encodeJson v = case v of
    Nearest -> "tag" := "Nearest" ~> jsonEmptyObject
    Linear -> "tag" := "Linear" ~> jsonEmptyObject
    NearestMipmapNearest -> "tag" := "NearestMipmapNearest" ~> jsonEmptyObject
    NearestMipmapLinear -> "tag" := "NearestMipmapLinear" ~> jsonEmptyObject
    LinearMipmapNearest -> "tag" := "LinearMipmapNearest" ~> jsonEmptyObject
    LinearMipmapLinear -> "tag" := "LinearMipmapLinear" ~> jsonEmptyObject

instance decodeJsonFilter :: DecodeJson Filter where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Nearest" -> pure Nearest
      "Linear" -> pure Linear
      "NearestMipmapNearest" -> pure NearestMipmapNearest
      "NearestMipmapLinear" -> pure NearestMipmapLinear
      "LinearMipmapNearest" -> pure LinearMipmapNearest
      "LinearMipmapLinear" -> pure LinearMipmapLinear
      _ -> Left ("decodeJsonFilter - unknown tag: " <> tag)

instance encodeJsonEdgeMode :: EncodeJson EdgeMode where
  encodeJson v = case v of
    Repeat -> "tag" := "Repeat" ~> jsonEmptyObject
    MirroredRepeat -> "tag" := "MirroredRepeat" ~> jsonEmptyObject
    ClampToEdge -> "tag" := "ClampToEdge" ~> jsonEmptyObject
    ClampToBorder -> "tag" := "ClampToBorder" ~> jsonEmptyObject

instance decodeJsonEdgeMode :: DecodeJson EdgeMode where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Repeat" -> pure Repeat
      "MirroredRepeat" -> pure MirroredRepeat
      "ClampToEdge" -> pure ClampToEdge
      "ClampToBorder" -> pure ClampToBorder
      _ -> Left ("decodeJsonEdgeMode - unknown tag: " <> tag)

instance encodeJsonImageSemantic :: EncodeJson ImageSemantic where
  encodeJson v = case v of
    Depth -> "tag" := "Depth" ~> jsonEmptyObject
    Stencil -> "tag" := "Stencil" ~> jsonEmptyObject
    Color -> "tag" := "Color" ~> jsonEmptyObject

instance decodeJsonImageSemantic :: DecodeJson ImageSemantic where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Depth" -> pure Depth
      "Stencil" -> pure Stencil
      "Color" -> pure Color
      _ -> Left ("decodeJsonImageSemantic - unknown tag: " <> tag)

instance encodeJsonImageRef :: EncodeJson ImageRef where
  encodeJson v = case v of
    TextureImage arg0 arg1 arg2 -> "tag" := "TextureImage" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> "arg2" := arg2 ~> jsonEmptyObject
    Framebuffer arg0 -> "tag" := "Framebuffer" ~> "arg0" := arg0 ~> jsonEmptyObject

instance decodeJsonImageRef :: DecodeJson ImageRef where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TextureImage" -> TextureImage <$> obj .? "arg0" <*> obj .? "arg1" <*> obj .? "arg2"
      "Framebuffer" -> Framebuffer <$> obj .? "arg0"
      _ -> Left ("decodeJsonImageRef - unknown tag: " <> tag)

instance encodeJsonClearImage :: EncodeJson ClearImage where
  encodeJson v = case v of
    ClearImage r ->
      "tag" := "ClearImage" ~>
      "imageSemantic" := r.imageSemantic ~>
      "clearValue" := r.clearValue ~>
      jsonEmptyObject

instance decodeJsonClearImage :: DecodeJson ClearImage where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "ClearImage" -> do
        imageSemantic <- obj .? "imageSemantic"
        clearValue <- obj .? "clearValue"
        pure $ ClearImage
          { imageSemantic:imageSemantic
          , clearValue:clearValue
          } 
      _ -> Left ("decodeJsonClearImage - unknown tag: " <> tag)

instance encodeJsonCommand :: EncodeJson Command where
  encodeJson v = case v of
    SetRasterContext arg0 -> "tag" := "SetRasterContext" ~> "arg0" := arg0 ~> jsonEmptyObject
    SetAccumulationContext arg0 -> "tag" := "SetAccumulationContext" ~> "arg0" := arg0 ~> jsonEmptyObject
    SetRenderTarget arg0 -> "tag" := "SetRenderTarget" ~> "arg0" := arg0 ~> jsonEmptyObject
    SetProgram arg0 -> "tag" := "SetProgram" ~> "arg0" := arg0 ~> jsonEmptyObject
    SetSamplerUniform arg0 arg1 -> "tag" := "SetSamplerUniform" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject
    SetTexture arg0 arg1 -> "tag" := "SetTexture" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject
    SetSampler arg0 arg1 -> "tag" := "SetSampler" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject
    RenderSlot arg0 -> "tag" := "RenderSlot" ~> "arg0" := arg0 ~> jsonEmptyObject
    RenderStream arg0 -> "tag" := "RenderStream" ~> "arg0" := arg0 ~> jsonEmptyObject
    ClearRenderTarget arg0 -> "tag" := "ClearRenderTarget" ~> "arg0" := arg0 ~> jsonEmptyObject
    GenerateMipMap arg0 -> "tag" := "GenerateMipMap" ~> "arg0" := arg0 ~> jsonEmptyObject
    SaveImage arg0 arg1 -> "tag" := "SaveImage" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject
    LoadImage arg0 arg1 -> "tag" := "LoadImage" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject

instance decodeJsonCommand :: DecodeJson Command where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "SetRasterContext" -> SetRasterContext <$> obj .? "arg0"
      "SetAccumulationContext" -> SetAccumulationContext <$> obj .? "arg0"
      "SetRenderTarget" -> SetRenderTarget <$> obj .? "arg0"
      "SetProgram" -> SetProgram <$> obj .? "arg0"
      "SetSamplerUniform" -> SetSamplerUniform <$> obj .? "arg0" <*> obj .? "arg1"
      "SetTexture" -> SetTexture <$> obj .? "arg0" <*> obj .? "arg1"
      "SetSampler" -> SetSampler <$> obj .? "arg0" <*> obj .? "arg1"
      "RenderSlot" -> RenderSlot <$> obj .? "arg0"
      "RenderStream" -> RenderStream <$> obj .? "arg0"
      "ClearRenderTarget" -> ClearRenderTarget <$> obj .? "arg0"
      "GenerateMipMap" -> GenerateMipMap <$> obj .? "arg0"
      "SaveImage" -> SaveImage <$> obj .? "arg0" <*> obj .? "arg1"
      "LoadImage" -> LoadImage <$> obj .? "arg0" <*> obj .? "arg1"
      _ -> Left ("decodeJsonCommand - unknown tag: " <> tag)

instance encodeJsonSamplerDescriptor :: EncodeJson SamplerDescriptor where
  encodeJson v = case v of
    SamplerDescriptor r ->
      "tag" := "SamplerDescriptor" ~>
      "samplerWrapS" := r.samplerWrapS ~>
      "samplerWrapT" := r.samplerWrapT ~>
      "samplerWrapR" := r.samplerWrapR ~>
      "samplerMinFilter" := r.samplerMinFilter ~>
      "samplerMagFilter" := r.samplerMagFilter ~>
      "samplerBorderColor" := r.samplerBorderColor ~>
      "samplerMinLod" := r.samplerMinLod ~>
      "samplerMaxLod" := r.samplerMaxLod ~>
      "samplerLodBias" := r.samplerLodBias ~>
      "samplerCompareFunc" := r.samplerCompareFunc ~>
      jsonEmptyObject

instance decodeJsonSamplerDescriptor :: DecodeJson SamplerDescriptor where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "SamplerDescriptor" -> do
        samplerWrapS <- obj .? "samplerWrapS"
        samplerWrapT <- obj .? "samplerWrapT"
        samplerWrapR <- obj .? "samplerWrapR"
        samplerMinFilter <- obj .? "samplerMinFilter"
        samplerMagFilter <- obj .? "samplerMagFilter"
        samplerBorderColor <- obj .? "samplerBorderColor"
        samplerMinLod <- obj .? "samplerMinLod"
        samplerMaxLod <- obj .? "samplerMaxLod"
        samplerLodBias <- obj .? "samplerLodBias"
        samplerCompareFunc <- obj .? "samplerCompareFunc"
        pure $ SamplerDescriptor
          { samplerWrapS:samplerWrapS
          , samplerWrapT:samplerWrapT
          , samplerWrapR:samplerWrapR
          , samplerMinFilter:samplerMinFilter
          , samplerMagFilter:samplerMagFilter
          , samplerBorderColor:samplerBorderColor
          , samplerMinLod:samplerMinLod
          , samplerMaxLod:samplerMaxLod
          , samplerLodBias:samplerLodBias
          , samplerCompareFunc:samplerCompareFunc
          } 
      _ -> Left ("decodeJsonSamplerDescriptor - unknown tag: " <> tag)

instance encodeJsonTextureDescriptor :: EncodeJson TextureDescriptor where
  encodeJson v = case v of
    TextureDescriptor r ->
      "tag" := "TextureDescriptor" ~>
      "textureType" := r.textureType ~>
      "textureSize" := r.textureSize ~>
      "textureSemantic" := r.textureSemantic ~>
      "textureSampler" := r.textureSampler ~>
      "textureBaseLevel" := r.textureBaseLevel ~>
      "textureMaxLevel" := r.textureMaxLevel ~>
      jsonEmptyObject

instance decodeJsonTextureDescriptor :: DecodeJson TextureDescriptor where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TextureDescriptor" -> do
        textureType <- obj .? "textureType"
        textureSize <- obj .? "textureSize"
        textureSemantic <- obj .? "textureSemantic"
        textureSampler <- obj .? "textureSampler"
        textureBaseLevel <- obj .? "textureBaseLevel"
        textureMaxLevel <- obj .? "textureMaxLevel"
        pure $ TextureDescriptor
          { textureType:textureType
          , textureSize:textureSize
          , textureSemantic:textureSemantic
          , textureSampler:textureSampler
          , textureBaseLevel:textureBaseLevel
          , textureMaxLevel:textureMaxLevel
          } 
      _ -> Left ("decodeJsonTextureDescriptor - unknown tag: " <> tag)

instance encodeJsonParameter :: EncodeJson Parameter where
  encodeJson v = case v of
    Parameter r ->
      "tag" := "Parameter" ~>
      "name" := r.name ~>
      "ty" := r.ty ~>
      jsonEmptyObject

instance decodeJsonParameter :: DecodeJson Parameter where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Parameter" -> do
        name <- obj .? "name"
        ty <- obj .? "ty"
        pure $ Parameter
          { name:name
          , ty:ty
          } 
      _ -> Left ("decodeJsonParameter - unknown tag: " <> tag)

instance encodeJsonProgram :: EncodeJson Program where
  encodeJson v = case v of
    Program r ->
      "tag" := "Program" ~>
      "programUniforms" := r.programUniforms ~>
      "programStreams" := r.programStreams ~>
      "programInTextures" := r.programInTextures ~>
      "programOutput" := r.programOutput ~>
      "vertexShader" := r.vertexShader ~>
      "geometryShader" := r.geometryShader ~>
      "fragmentShader" := r.fragmentShader ~>
      jsonEmptyObject

instance decodeJsonProgram :: DecodeJson Program where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Program" -> do
        programUniforms <- obj .? "programUniforms"
        programStreams <- obj .? "programStreams"
        programInTextures <- obj .? "programInTextures"
        programOutput <- obj .? "programOutput"
        vertexShader <- obj .? "vertexShader"
        geometryShader <- obj .? "geometryShader"
        fragmentShader <- obj .? "fragmentShader"
        pure $ Program
          { programUniforms:programUniforms
          , programStreams:programStreams
          , programInTextures:programInTextures
          , programOutput:programOutput
          , vertexShader:vertexShader
          , geometryShader:geometryShader
          , fragmentShader:fragmentShader
          } 
      _ -> Left ("decodeJsonProgram - unknown tag: " <> tag)

instance encodeJsonSlot :: EncodeJson Slot where
  encodeJson v = case v of
    Slot r ->
      "tag" := "Slot" ~>
      "slotName" := r.slotName ~>
      "slotStreams" := r.slotStreams ~>
      "slotUniforms" := r.slotUniforms ~>
      "slotPrimitive" := r.slotPrimitive ~>
      "slotPrograms" := r.slotPrograms ~>
      jsonEmptyObject

instance decodeJsonSlot :: DecodeJson Slot where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Slot" -> do
        slotName <- obj .? "slotName"
        slotStreams <- obj .? "slotStreams"
        slotUniforms <- obj .? "slotUniforms"
        slotPrimitive <- obj .? "slotPrimitive"
        slotPrograms <- obj .? "slotPrograms"
        pure $ Slot
          { slotName:slotName
          , slotStreams:slotStreams
          , slotUniforms:slotUniforms
          , slotPrimitive:slotPrimitive
          , slotPrograms:slotPrograms
          } 
      _ -> Left ("decodeJsonSlot - unknown tag: " <> tag)

instance encodeJsonStreamData :: EncodeJson StreamData where
  encodeJson v = case v of
    StreamData r ->
      "tag" := "StreamData" ~>
      "streamData" := r.streamData ~>
      "streamType" := r.streamType ~>
      "streamPrimitive" := r.streamPrimitive ~>
      "streamPrograms" := r.streamPrograms ~>
      jsonEmptyObject

instance decodeJsonStreamData :: DecodeJson StreamData where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "StreamData" -> do
        streamData <- obj .? "streamData"
        streamType <- obj .? "streamType"
        streamPrimitive <- obj .? "streamPrimitive"
        streamPrograms <- obj .? "streamPrograms"
        pure $ StreamData
          { streamData:streamData
          , streamType:streamType
          , streamPrimitive:streamPrimitive
          , streamPrograms:streamPrograms
          } 
      _ -> Left ("decodeJsonStreamData - unknown tag: " <> tag)

instance encodeJsonTargetItem :: EncodeJson TargetItem where
  encodeJson v = case v of
    TargetItem r ->
      "tag" := "TargetItem" ~>
      "targetSemantic" := r.targetSemantic ~>
      "targetRef" := r.targetRef ~>
      jsonEmptyObject

instance decodeJsonTargetItem :: DecodeJson TargetItem where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TargetItem" -> do
        targetSemantic <- obj .? "targetSemantic"
        targetRef <- obj .? "targetRef"
        pure $ TargetItem
          { targetSemantic:targetSemantic
          , targetRef:targetRef
          } 
      _ -> Left ("decodeJsonTargetItem - unknown tag: " <> tag)

instance encodeJsonRenderTarget :: EncodeJson RenderTarget where
  encodeJson v = case v of
    RenderTarget r ->
      "tag" := "RenderTarget" ~>
      "renderTargets" := r.renderTargets ~>
      jsonEmptyObject

instance decodeJsonRenderTarget :: DecodeJson RenderTarget where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "RenderTarget" -> do
        renderTargets <- obj .? "renderTargets"
        pure $ RenderTarget
          { renderTargets:renderTargets
          } 
      _ -> Left ("decodeJsonRenderTarget - unknown tag: " <> tag)

instance encodeJsonBackend :: EncodeJson Backend where
  encodeJson v = case v of
    WebGL1 -> "tag" := "WebGL1" ~> jsonEmptyObject
    OpenGL33 -> "tag" := "OpenGL33" ~> jsonEmptyObject

instance decodeJsonBackend :: DecodeJson Backend where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "WebGL1" -> pure WebGL1
      "OpenGL33" -> pure OpenGL33
      _ -> Left ("decodeJsonBackend - unknown tag: " <> tag)

instance encodeJsonPipeline :: EncodeJson Pipeline where
  encodeJson v = case v of
    Pipeline r ->
      "tag" := "Pipeline" ~>
      "info" := r.info ~>
      "backend" := r.backend ~>
      "textures" := r.textures ~>
      "samplers" := r.samplers ~>
      "targets" := r.targets ~>
      "programs" := r.programs ~>
      "slots" := r.slots ~>
      "streams" := r.streams ~>
      "commands" := r.commands ~>
      jsonEmptyObject

instance decodeJsonPipeline :: DecodeJson Pipeline where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Pipeline" -> do
        info <- obj .? "info"
        backend <- obj .? "backend"
        textures <- obj .? "textures"
        samplers <- obj .? "samplers"
        targets <- obj .? "targets"
        programs <- obj .? "programs"
        slots <- obj .? "slots"
        streams <- obj .? "streams"
        commands <- obj .? "commands"
        pure $ Pipeline
          { info:info
          , backend:backend
          , textures:textures
          , samplers:samplers
          , targets:targets
          , programs:programs
          , slots:slots
          , streams:streams
          , commands:commands
          } 
      _ -> Left ("decodeJsonPipeline - unknown tag: " <> tag)

