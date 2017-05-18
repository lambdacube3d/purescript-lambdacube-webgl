module LambdaCube.WebGL.Data where

import Prelude
import Control.Monad.Eff.Exception (error, throwException)

import Graphics.WebGLRaw as GL
import Graphics.WebGLTexture as GLTex
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Array (length, last, (!!), zip, take)
--import Data.Foldable
import Data.Traversable (for, for_, scanl)

import LambdaCube.WebGL.Type (ArrayType(..), Buffer, GFX, LCArray(..), TextureData(..), sizeOfArrayType)
import LambdaCube.WebGL.Util (arrayTypeToGLType, bufferDataAlloc, bufferSubDataArrayBuffer, bufferSubDataArrayView, loadImage_, newArrayBuffer, newFloatView, newInt16View, newInt8View, newWord16View, newWord8View, nullWebGLBuffer, setArrayView, texImage2D__)

compileBuffer :: forall a. Array LCArray -> GFX (Buffer a)
compileBuffer arrs = do
    let offsets = [0] `append` scanl (\s (Array t a) -> s + sizeOfArrayType t * length a) 0 arrs -- BUG:  scanl (+) 0 [1,1] == [1,2] =!= [0,1,2]
        size = case last offsets of
          Just s  -> s
          Nothing -> 0
    b <- newArrayBuffer size
    descs <- for (zip arrs $ take (length arrs) offsets) $ \(Tuple (Array t a) o) -> do
      let len     = length a
          bytes   = len * sizeOfArrayType t
          newView = case t of
            ArrWord8  -> newWord8View
            ArrWord16 -> newWord16View
            ArrInt8   -> newInt8View
            ArrInt16  -> newInt16View
            ArrFloat  -> newFloatView
      view <- newView b o len
      setArrayView view a
      pure {arrType: t, arrLength: len, arrOffset: o, arrSize: bytes, arrView: view}

    bo <- GL.createBuffer_
    GL.bindBuffer_ GL._ARRAY_BUFFER bo
    bufferDataAlloc GL._ARRAY_BUFFER size GL._STATIC_DRAW
    bufferSubDataArrayBuffer GL._ARRAY_BUFFER 0 b
    GL.bindBuffer_ GL._ARRAY_BUFFER nullWebGLBuffer
    pure {arrays: descs, glBuffer: bo, buffer: b}

updateBuffer :: forall a. Buffer a -> Array (Tuple Int LCArray) -> GFX Unit
updateBuffer b arrs = do
  for_ arrs $ \(Tuple i (Array t a)) -> case b.arrays !! i of
    Nothing -> throwException $ error "wrong index"
    Just d  -> do
      when (arrayTypeToGLType t /= arrayTypeToGLType d.arrType) $ throwException $ error "type mismatch"
      when (length a /= d.arrLength) $ throwException $ error "size mismatch"
      setArrayView d.arrView a
      bufferSubDataArrayView GL._ARRAY_BUFFER d.arrOffset d.arrView

uploadTexture2DToGPU :: String -> (TextureData -> GFX Unit) -> GFX Unit
uploadTexture2DToGPU name action = do
  to <- GL.createTexture_
  loadImage_ name \image -> do
    -- FIXME: basic implementation, the filter should be controlled by the pipeline not by the input
    GL.bindTexture_ GL._TEXTURE_2D to
    GL.texParameteri_ GL._TEXTURE_2D GL._TEXTURE_MAG_FILTER GL._LINEAR
    GL.texParameteri_ GL._TEXTURE_2D GL._TEXTURE_MIN_FILTER GL._LINEAR
    GL.pixelStorei_ GL._UNPACK_FLIP_Y_WEBGL 1
    GL.pixelStorei_ GL._UNPACK_PREMULTIPLY_ALPHA_WEBGL 0
    texImage2D__ GL._TEXTURE_2D 0 GL._RGBA GL._RGBA GL._UNSIGNED_BYTE image
    GLTex.unbindTexture GLTex.TEXTURE_2D

    action $ TextureData to
