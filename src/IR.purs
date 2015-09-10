-- generated file, do not modify!
-- 2015-09-10T11:12:49.882779000000Z

module IR where
import Prelude
import Data.Generic
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap(..))
import Data.Map (Map(..))
import Data.List (List(..))
import Linear

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


data StencilTests
  = StencilTests StencilTest StencilTest

data StencilTest
  = StencilTest
  { stencilComparision :: ComparisonFunction
  , stencilReference :: Int32
  , stencilMask :: Word32
  }


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

data ImageRef
  = TextureImage TextureName Int (Maybe Int)
  | Framebuffer ImageSemantic

data ImageSemantic
  = Depth
  | Stencil
  | Color

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

data TextureDescriptor
  = TextureDescriptor
  { textureType :: TextureType
  , textureSize :: Value
  , textureSemantic :: ImageSemantic
  , textureSampler :: SamplerDescriptor
  , textureBaseLevel :: Int
  , textureMaxLevel :: Int
  }


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
  { backend :: Backend
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
instance eqColorArity   :: Eq ColorArity   where eq = gEq

derive instance genericTextureDataType :: Generic TextureDataType
instance showTextureDataType :: Show TextureDataType where show = gShow
instance eqTextureDataType   :: Eq TextureDataType   where eq = gEq

