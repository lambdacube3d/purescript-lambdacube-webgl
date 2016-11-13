module Data where

import Prelude
import Control.Monad.Eff.Console as C
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Exception

import Graphics.WebGLRaw as GL
import Graphics.WebGLTexture as GLTex
import Data.Maybe
import Data.Tuple
import Data.Array
import Data.Foldable
import Data.Traversable

import IR
import LinearBase
import Type
import Util

compileBuffer :: Array LCArray -> GFX Buffer
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

updateBuffer :: Buffer -> Array (Tuple Int LCArray) -> GFX Unit
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
    -- HINT: basic implementation
    GLTex.handleLoad2D (GLTex.WebGLTex to) GLTex.MIPMAP image
    action $ TextureData to
