module Util where

import Prelude
import Control.Monad.Eff.Console as C
import Graphics.WebGLRaw as GL
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Control.Monad.Eff.WebGL
import Data.Tuple
import Data.Maybe
import Data.Array
import Data.List (List(..))
import Data.Unfoldable (replicate)
import Data.ArrayBuffer.Types as AB
import Data.TypedArray as TA
import Math
import Data.Foldable
import Partial.Unsafe (unsafeCrashWith)

import IR
import LinearBase
import Type

comparisonFunctionToGLType :: ComparisonFunction -> GL.GLenum
comparisonFunctionToGLType a = case a of
    Always      -> GL._ALWAYS
    Equal       -> GL._EQUAL
    Gequal      -> GL._GEQUAL
    Greater     -> GL._GREATER
    Lequal      -> GL._LEQUAL
    Less        -> GL._LESS
    Never       -> GL._NEVER
    Notequal    -> GL._NOTEQUAL

blendEquationToGLType :: BlendEquation -> GL.GLenum
blendEquationToGLType a = case a of
    FuncAdd             -> GL._FUNC_ADD
    FuncReverseSubtract -> GL._FUNC_REVERSE_SUBTRACT
    FuncSubtract        -> GL._FUNC_SUBTRACT
    Max                 -> GL._FUNC_ADD -- _MAX -- not presented
    Min                 -> GL._FUNC_ADD -- _MIN-- not presented

blendingFactorToGLType :: BlendingFactor -> GL.GLenum
blendingFactorToGLType a = case a of
    ConstantAlpha           -> GL._CONSTANT_ALPHA
    ConstantColor           -> GL._CONSTANT_COLOR
    DstAlpha                -> GL._DST_ALPHA
    DstColor                -> GL._DST_COLOR
    One                     -> GL._ONE
    OneMinusConstantAlpha   -> GL._ONE_MINUS_CONSTANT_ALPHA
    OneMinusConstantColor   -> GL._ONE_MINUS_CONSTANT_COLOR
    OneMinusDstAlpha        -> GL._ONE_MINUS_DST_ALPHA
    OneMinusDstColor        -> GL._ONE_MINUS_DST_COLOR
    OneMinusSrcAlpha        -> GL._ONE_MINUS_SRC_ALPHA
    OneMinusSrcColor        -> GL._ONE_MINUS_SRC_COLOR
    SrcAlpha                -> GL._SRC_ALPHA
    SrcAlphaSaturate        -> GL._SRC_ALPHA_SATURATE
    SrcColor                -> GL._SRC_COLOR
    Zero                    -> GL._ZERO

toStreamType :: InputType -> GFX StreamType
toStreamType a = case a of
  Float -> pure TFloat
  V2F   -> pure TV2F
  V3F   -> pure TV3F
  V4F   -> pure TV4F
  M22F  -> pure TM22F
  M33F  -> pure TM33F
  M44F  -> pure TM44F
  _     -> throwException $ error "invalid Stream Type"

foreign import setFloatArray :: AB.Float32Array -> Array Float -> GFX Unit
foreign import setIntArray :: AB.Int32Array -> Array Int -> GFX Unit
foreign import nullWebGLTexture :: GL.WebGLTexture

mkUniformSetter :: InputType -> GFX (Tuple GLUniform InputSetter)
mkUniformSetter t@Bool  = let r = TA.asInt32Array [0]                in pure $ Tuple (UniBool  r) (SBool  $ setIntArray r <<< toIntArray)
mkUniformSetter t@V2B   = let r = TA.asInt32Array (replicate 2 0)    in pure $ Tuple (UniV2B   r) (SV2B   $ setIntArray r <<< toIntArray)
mkUniformSetter t@V3B   = let r = TA.asInt32Array (replicate 3 0)    in pure $ Tuple (UniV3B   r) (SV3B   $ setIntArray r <<< toIntArray)
mkUniformSetter t@V4B   = let r = TA.asInt32Array (replicate 4 0)    in pure $ Tuple (UniV4B   r) (SV4B   $ setIntArray r <<< toIntArray)
mkUniformSetter t@Int   = let r = TA.asInt32Array [0]                in pure $ Tuple (UniInt   r) (SInt   $ setIntArray r <<< toIntArray)
mkUniformSetter t@V2I   = let r = TA.asInt32Array (replicate 2 0)    in pure $ Tuple (UniV2I   r) (SV2I   $ setIntArray r <<< toIntArray)
mkUniformSetter t@V3I   = let r = TA.asInt32Array (replicate 3 0)    in pure $ Tuple (UniV3I   r) (SV3I   $ setIntArray r <<< toIntArray)
mkUniformSetter t@V4I   = let r = TA.asInt32Array (replicate 4 0)    in pure $ Tuple (UniV4I   r) (SV4I   $ setIntArray r <<< toIntArray)
mkUniformSetter t@Float = let r = TA.asFloat32Array [0.0]              in pure $ Tuple (UniFloat r) (SFloat $ setFloatArray r <<< toArray)
mkUniformSetter t@V2F   = let r = TA.asFloat32Array (replicate 2 0.0)  in pure $ Tuple (UniV2F   r) (SV2F   $ setFloatArray r <<< toArray)
mkUniformSetter t@V3F   = let r = TA.asFloat32Array (replicate 3 0.0)  in pure $ Tuple (UniV3F   r) (SV3F   $ setFloatArray r <<< toArray)
mkUniformSetter t@V4F   = let r = TA.asFloat32Array (replicate 4 0.0)  in pure $ Tuple (UniV4F   r) (SV4F   $ setFloatArray r <<< toArray)
mkUniformSetter t@M22F  = let r = TA.asFloat32Array (replicate 4 0.0)  in pure $ Tuple (UniM22F  r) (SM22F  $ setFloatArray r <<< toArray)
mkUniformSetter t@M33F  = let r = TA.asFloat32Array (replicate 9 0.0)  in pure $ Tuple (UniM33F  r) (SM33F  $ setFloatArray r <<< toArray)
mkUniformSetter t@M44F  = let r = TA.asFloat32Array (replicate 16 0.0) in pure $ Tuple (UniM44F  r) (SM44F  $ setFloatArray r <<< toArray)
mkUniformSetter t@FTexture2D = do
  r <- newRef (TextureData nullWebGLTexture)
  pure $ Tuple (UniFTexture2D r) (SFTexture2D $ writeRef r)
mkUniformSetter _ = unsafeCrashWith "mkUniformSetter"

primitiveToFetchPrimitive :: Primitive -> FetchPrimitive
primitiveToFetchPrimitive prim = case prim of
  TriangleStrip           -> Triangles
  TriangleList            -> Triangles
  TriangleFan             -> Triangles
  LineStrip               -> Lines
  LineLoop                -> Lines
  LineList                -> Lines
  PointList               -> Points

unlines :: Array String -> String
unlines l = case uncons l of
  Nothing -> ""
  Just a  -> if null a.tail then a.head else a.head <> "\n" <> unlines a.tail

setVertexAttrib :: GL.GLuint -> Stream Buffer -> GFX Unit
setVertexAttrib i val = case val of
  ConstFloat v -> setAFloat i v
  ConstV2F v   -> setAV2F i v
  ConstV3F v   -> setAV3F i v
  ConstV4F v   -> setAV4F i v
  ConstM22F (V2 x y) -> do
    setAV2F i x
    setAV2F (i+1) y
  ConstM33F (V3 x y z) -> do
    setAV3F i x
    setAV3F (i+1) y
    setAV3F (i+2) z
  ConstM44F (V4 x y z w) -> do
    setAV4F i x
    setAV4F (i+1) y
    setAV4F (i+2) z
    setAV4F (i+3) w
  _ -> throwException $ error "internal error (setVertexAttrib)!"

setAFloat :: GL.GLuint -> Float -> GFX Unit
setAFloat i v = GL.vertexAttrib1f_ i v

setAV2F :: GL.GLuint -> V2F -> GFX Unit
setAV2F i (V2 x y) = GL.vertexAttrib2f_ i x y

setAV3F :: GL.GLuint -> V3F -> GFX Unit
setAV3F i (V3 x y z) = GL.vertexAttrib3f_ i x y z

setAV4F :: GL.GLuint -> V4F -> GFX Unit
setAV4F i (V4 x y z w) = GL.vertexAttrib4f_ i x y z w

{-
foreign import uniform1iv_:: forall eff. Fn2 WebGLUniformLocation
                                             Int32Array
                                             (Eff (webgl :: WebGl | eff) Unit)

  | uni.uType == _FLOAT_VEC4    = uniform4fv_ uni.uLocation (asArrayBuffer value)
-}
-- sets value based uniforms only (does not handle textures)
--setUniform :: forall a . GL.WebGLUniformLocation -> GLUniform -> GFX Unit
setUniform i uni = case uni of
  UniBool  r -> GL.uniform1iv_ i r
  UniV2B   r -> GL.uniform2iv_ i r
  UniV3B   r -> GL.uniform3iv_ i r
  UniV4B   r -> GL.uniform4iv_ i r
  UniInt   r -> GL.uniform1iv_ i r
  UniV2I   r -> GL.uniform2iv_ i r
  UniV3I   r -> GL.uniform3iv_ i r
  UniV4I   r -> GL.uniform4iv_ i r
  UniFloat r -> GL.uniform1fv_ i r
  UniV2F   r -> GL.uniform2fv_ i r
  UniV3F   r -> GL.uniform3fv_ i r
  UniV4F   r -> GL.uniform4fv_ i r
  UniM22F  r -> GL.uniformMatrix2fv_ i false r
  UniM33F  r -> GL.uniformMatrix3fv_ i false r
  UniM44F  r -> GL.uniformMatrix4fv_ i false r
  UniFTexture2D r -> pure unit
  _ -> throwException $ error "internal error (setUniform)!"

primitiveToGLType :: Primitive -> GL.GLenum
primitiveToGLType p = case p of
  TriangleStrip -> GL._TRIANGLE_STRIP
  TriangleList  -> GL._TRIANGLES
  TriangleFan   -> GL._TRIANGLE_FAN
  LineStrip     -> GL._LINE_STRIP
  LineLoop      -> GL._LINE_LOOP
  LineList      -> GL._LINES
  PointList     -> GL._POINTS

arrayTypeToGLType :: ArrayType -> GL.GLenum
arrayTypeToGLType a = case a of
  ArrWord8    -> GL._UNSIGNED_BYTE
  ArrWord16   -> GL._UNSIGNED_SHORT
  ArrInt8     -> GL._BYTE
  ArrInt16    -> GL._SHORT
  ArrFloat    -> GL._FLOAT

-- custom typed array buffer and view functions
foreign import newArrayBuffer :: Int -> GFX ArrayBuffer
foreign import newWord8View :: ArrayBuffer -> Int -> Int -> GFX ArrayView
foreign import newWord16View :: ArrayBuffer -> Int -> Int -> GFX ArrayView
foreign import newInt8View :: ArrayBuffer -> Int -> Int -> GFX ArrayView
foreign import newInt16View :: ArrayBuffer -> Int -> Int -> GFX ArrayView
foreign import newFloatView :: ArrayBuffer -> Int -> Int -> GFX ArrayView
foreign import setArrayView :: ArrayView -> Array Number -> GFX Unit
foreign import nullWebGLBuffer :: GL.WebGLBuffer
foreign import bufferDataAlloc :: forall eff. GL.GLenum -> Int -> GL.GLenum -> GFX Unit
foreign import bufferSubDataArrayBuffer :: forall eff. GL.GLenum -> GL.GLintptr -> ArrayBuffer -> GFX Unit
foreign import bufferSubDataArrayView :: forall eff. GL.GLenum -> GL.GLintptr -> ArrayView -> GFX Unit

compileTexture :: TextureDescriptor -> GFX GLTexture
compileTexture (TextureDescriptor txD) = do
  to <- GL.createTexture_
  let div a b = floor $ a / b
      mipSize 0 x = [x]
      mipSize n x = x : mipSize (n-1) (x / 2)
      mipS = mipSize (txD.textureMaxLevel - txD.textureBaseLevel)
      levels = txD.textureBaseLevel..txD.textureMaxLevel
      txSetup txTarget dTy = do
          internalFormat  <- textureDataTypeToGLType txD.textureSemantic dTy
          dataFormat      <- textureDataTypeToGLArityType txD.textureSemantic dTy
          GL.bindTexture_ txTarget to
          setTextureSamplerParameters txTarget txD.textureSampler
          pure $ Tuple internalFormat dataFormat
  let act = case txD.textureType of
        Texture2D dTy 1 -> do
          let txTarget = GL._TEXTURE_2D
          case txD.textureSize of
           VV2U (V2 txW txH) -> do
            Tuple internalFormat dataFormat <- txSetup txTarget dTy
            for_ (zip levels (zip (mipS txW) (mipS txH))) $ \(Tuple l (Tuple w h)) -> do
              texImage2DNull_ txTarget l internalFormat w h 0 dataFormat (if dataFormat == GL._DEPTH_COMPONENT then GL._UNSIGNED_SHORT else GL._UNSIGNED_BYTE)
              pure unit
            pure $
              { textureObject: to
              , textureTarget: GL._TEXTURE_2D --target
              }
           _ -> unsafeCrashWith "compileTexture"
        _ -> throwException $ error "Unsupported texture type!"
  act

textureDataTypeToGLType :: ImageSemantic -> TextureDataType -> GFX GL.GLenum
textureDataTypeToGLType Color a = case a of
    FloatT RGBA -> pure GL._RGBA
    IntT   RGBA -> pure GL._RGBA
    WordT  RGBA -> pure GL._RGBA
    a           -> throwException $ error $ "FIXME: This texture format is not yet supported" <> show a
textureDataTypeToGLType Depth a = case a of
    FloatT Red  -> pure GL._DEPTH_COMPONENT
    WordT  Red  -> pure GL._DEPTH_COMPONENT
    a           -> throwException $ error $ "FIXME: This texture format is not yet supported" <> show a
textureDataTypeToGLType Stencil a = case a of
    a           -> throwException $ error $ "FIXME: This texture format is not yet supported" <> show a

textureDataTypeToGLArityType :: ImageSemantic -> TextureDataType -> GFX GL.GLenum
textureDataTypeToGLArityType Color a = case a of
    FloatT RGBA -> pure GL._RGBA
    IntT   RGBA -> pure GL._RGBA
    WordT  RGBA -> pure GL._RGBA
    a           -> throwException $ error $ "FIXME: This texture format is not yet supported" <> show a
textureDataTypeToGLArityType Depth a = case a of
    FloatT Red  -> pure GL._DEPTH_COMPONENT
    WordT  Red  -> pure GL._DEPTH_COMPONENT
    a           -> throwException $ error $ "FIXME: This texture format is not yet supported" <> show a
textureDataTypeToGLArityType Stencil a = case a of
    a           -> throwException $ error $ "FIXME: This texture format is not yet supported" <> show a

foreign import texImage2DNull_
    :: GL.GLenum->
       GL.GLint->
       GL.GLenum->
       GL.GLsizei->
       GL.GLsizei->
       GL.GLint->
       GL.GLenum->
       GL.GLenum->
       GFX Unit

setTextureSamplerParameters :: GL.GLenum -> SamplerDescriptor -> GFX Unit
setTextureSamplerParameters t (SamplerDescriptor s) = do
    GL.texParameteri_ t GL._TEXTURE_WRAP_S $ edgeModeToGLType s.samplerWrapS
    case s.samplerWrapT of
        Nothing -> pure unit
        Just a  -> GL.texParameteri_ t GL._TEXTURE_WRAP_T $ edgeModeToGLType a
    GL.texParameteri_ t GL._TEXTURE_MIN_FILTER $ filterToGLType s.samplerMinFilter
    GL.texParameteri_ t GL._TEXTURE_MAG_FILTER $ filterToGLType s.samplerMagFilter

filterToGLType :: Filter -> GL.GLenum
filterToGLType a = case a of
    Nearest                 -> GL._NEAREST
    Linear                  -> GL._LINEAR
    NearestMipmapNearest    -> GL._NEAREST_MIPMAP_NEAREST
    NearestMipmapLinear     -> GL._NEAREST_MIPMAP_LINEAR
    LinearMipmapNearest     -> GL._LINEAR_MIPMAP_NEAREST
    LinearMipmapLinear      -> GL._LINEAR_MIPMAP_LINEAR

edgeModeToGLType :: EdgeMode -> GL.GLenum
edgeModeToGLType a = case a of
    Repeat          -> GL._REPEAT
    MirroredRepeat  -> GL._MIRRORED_REPEAT
    ClampToEdge     -> GL._CLAMP_TO_EDGE
    _ -> unsafeCrashWith "edgeModeToGLType"

foreign import loadImage_ :: forall a . String -> (GLImageData -> GFX a) -> GFX Unit
