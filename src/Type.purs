module Type where

import Prelude
import Control.Monad.Eff
import qualified Control.Monad.Eff.Console as C
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception
import Control.Monad.Eff.WebGL
import qualified Data.StrMap as StrMap
import qualified Graphics.WebGLRaw as GL

import qualified Data.Map as Map
import Data.Tuple
import Data.Maybe
import Data.Array
import Data.Int
import qualified Data.ArrayBuffer.Types as AB

import IR
import Linear

type GFX a = forall e . Eff (webgl :: WebGl, console :: C.CONSOLE, err :: EXCEPTION, ref :: REF | e) a

type IntMap a = Map.Map Int a

foreign import data ArrayBuffer :: *
foreign import data ArrayView :: *

type Buffer = -- internal type
    { arrays    :: Array ArrayDesc
    , glBuffer  :: GL.WebGLBuffer
    , buffer    :: ArrayBuffer
    }

type ArrayDesc =
    { arrType   :: ArrayType
    , arrLength :: Int  -- item count
    , arrOffset :: Int  -- byte position in buffer
    , arrSize   :: Int  -- size in bytes
    , arrView   :: ArrayView
    }

data ArrayType
    = ArrWord8
    | ArrWord16
    | ArrInt8
    | ArrInt16
    | ArrFloat

sizeOfArrayType :: ArrayType -> Int
sizeOfArrayType ArrWord8  = 1
sizeOfArrayType ArrWord16 = 2
sizeOfArrayType ArrInt8   = 1
sizeOfArrayType ArrInt16  = 2
sizeOfArrayType ArrFloat  = 4

-- describes an array in a buffer
data LCArray = Array ArrayType (Array Number)

data StreamType
    = TFloat
    | TV2F
    | TV3F
    | TV4F
    | TM22F
    | TM33F
    | TM44F 

instance eqStreamType :: Eq (StreamType) where
  eq TFloat TFloat = true
  eq TV2F   TV2F   = true
  eq TV3F   TV3F   = true
  eq TV4F   TV4F   = true
  eq TM22F  TM22F  = true
  eq TM33F  TM33F  = true
  eq TM44F  TM44F  = true
  eq _      _      = false

instance showStreamType :: Show (StreamType) where
  show TFloat = "TFloat"
  show TV2F   = "TV2F"
  show TV3F   = "TV3F"
  show TV4F   = "TV4F"
  show TM22F  = "TM22F"
  show TM33F  = "TM33F"
  show TM44F  = "TM44F"

data Stream b
    = ConstFloat Float
    | ConstV2F   V2F
    | ConstV3F   V3F
    | ConstV4F   V4F
    | ConstM22F  M22F
    | ConstM33F  M33F
    | ConstM44F  M44F
    | Stream 
        { sType   :: StreamType
        , buffer  :: b
        , arrIdx  :: Int
        , start   :: Int
        , length  :: Int
        }

type IndexStream b =
    { buffer   :: b
    , arrIdx   :: Int
    , start    :: Int
    , length   :: Int
    }

data Primitive
    = TriangleStrip
    | TriangleList
    | TriangleFan
    | LineStrip
    | LineList
    | LineLoop
    | PointList

instance showPrimitive :: Show (Primitive) where
  show TriangleStrip = "TriangleStrip"
  show TriangleList  = "TriangleList"
  show TriangleFan   = "TriangleFan"
  show LineStrip     = "LineStrip"
  show LineList      = "LineList"
  show LineLoop      = "LineLoop"
  show PointList     = "PointList"

type SlotSchema =
    { primitive     :: FetchPrimitive
    , attributes    :: StrMap.StrMap StreamType
    }

type PipelineSchema =
    { slots     :: StrMap.StrMap SlotSchema
    , uniforms  :: StrMap.StrMap InputType
    }

data OrderJob
    = Generate
    | Reorder
    | Ordered

type GLSlot =
    { objectMap     :: IntMap GLObject
    , sortedObjects :: Array (Tuple Int GLObject)
    , orderJob      :: OrderJob
    }

data GLUniform
  = UniBool  AB.Int32Array
  | UniV2B   AB.Int32Array
  | UniV3B   AB.Int32Array
  | UniV4B   AB.Int32Array
  | UniInt   AB.Int32Array
  | UniV2I   AB.Int32Array
  | UniV3I   AB.Int32Array
  | UniV4I   AB.Int32Array
  | UniFloat AB.Float32Array
  | UniV2F   AB.Float32Array
  | UniV3F   AB.Float32Array
  | UniV4F   AB.Float32Array
  | UniM22F  AB.Float32Array
  | UniM33F  AB.Float32Array
  | UniM44F  AB.Float32Array

type WebGLPipelineInput =
    { schema        :: PipelineSchema
    , slotMap       :: StrMap.StrMap Int
    , slotVector    :: Array (Ref GLSlot)
    , objSeed       :: Ref Int
    , uniformSetter :: StrMap.StrMap InputSetter
    , uniformSetup  :: StrMap.StrMap GLUniform
    , screenSize    :: Ref V2U
    , pipelines     :: Ref (Array (Maybe WebGLPipeline)) -- attached pipelines
    }

type GLObject = -- internal type
    { slot       :: Int
    , primitive  :: Primitive
    , indices    :: Maybe (IndexStream Buffer)
    , attributes :: StrMap.StrMap (Stream Buffer)
    , uniSetter  :: StrMap.StrMap InputSetter
    , uniSetup   :: StrMap.StrMap GLUniform
    , order      :: Ref Int
    , enabled    :: Ref Bool
    , id         :: Int
    , commands   :: Ref (Array (Array (Array GLObjectCommand)))  -- pipeline id, program name, commands
    }

data InputConnection = InputConnection
  { id                      :: Int                -- identifier (vector index) for attached pipeline
  , input                   :: WebGLPipelineInput
  , slotMapPipelineToInput  :: Array SlotName         -- GLPipeline to GLPipelineInput slot name mapping
  , slotMapInputToPipeline  :: Array (Maybe SlotName)   -- GLPipelineInput to GLPipeline slot name mapping
  }

type GLStream =
  { commands    :: Ref (Array GLObjectCommand)
  , primitive   :: Primitive
  , attributes  :: StrMap.StrMap (Stream Buffer)
  , program     :: ProgramName
  }

type WebGLPipeline =
  { targets       :: Array GLRenderTarget
  , textures      :: Array GLTexture
  , programs      :: Array GLProgram
  , commands      :: Array Command
  , input         :: Ref (Maybe InputConnection)
  , slotNames     :: Array String
  , slotPrograms  :: Array (Array ProgramName) -- program list for every slot (programs depend on a slot)
  , curProgram    :: Ref (Maybe Int)
  , streams       :: Array GLStream
  }

type GLTexture =
    { textureObject   :: GL.WebGLTexture
    , textureTarget   :: GL.GLenum
    }

type GLRenderTarget =
    { framebufferObject         :: GL.WebGLFramebuffer
    , framebufferDrawbuffers    :: Maybe (Array GL.GLenum)
    }

type GLProgram =
  { program       :: GL.WebGLProgram
  , shaders       :: Array GL.WebGLShader
  , inputUniforms :: StrMap.StrMap GL.WebGLUniformLocation
  , inputSamplers :: StrMap.StrMap GL.WebGLUniformLocation
  , inputStreams  :: StrMap.StrMap {location :: GL.GLint, slotAttribute :: String}
  }

data GLObjectCommand
    = GLSetVertexAttribArray    GL.GLuint GL.WebGLBuffer GL.GLint GL.GLenum GL.GLintptr -- index buffer size type pointer
    | GLDrawArrays              GL.GLenum GL.GLint GL.GLsizei -- mode first count
    | GLDrawElements            GL.GLenum GL.GLsizei GL.GLenum GL.WebGLBuffer GL.GLintptr -- mode count type buffer indicesPtr
    | GLSetVertexAttrib         GL.GLuint (Stream Buffer) -- index value
    | GLSetUniform              GL.WebGLUniformLocation GLUniform
--    | GLBindTexture             !GLenum (IORef GLint) GLUniform               -- binds the texture from the gluniform to the specified texture unit and target

type SetterFun a = a -> GFX Unit

-- user will provide scalar input data via this type
data InputSetter
    = SBool  (SetterFun Bool)
    | SV2B   (SetterFun V2B)
    | SV3B   (SetterFun V3B)
    | SV4B   (SetterFun V4B)
    | SInt   (SetterFun Int32)
    | SV2I   (SetterFun V2I)
    | SV3I   (SetterFun V3I)
    | SV4I   (SetterFun V4I)
    | SFloat (SetterFun Float)
    | SV2F   (SetterFun V2F)
    | SV3F   (SetterFun V3F)
    | SV4F   (SetterFun V4F)
    | SM22F  (SetterFun M22F)
    | SM33F  (SetterFun M33F)
    | SM44F  (SetterFun M44F)

streamToStreamType :: forall a . Stream a -> StreamType
streamToStreamType s = case s of
    ConstFloat _ -> TFloat
    ConstV2F   _ -> TV2F
    ConstV3F   _ -> TV3F
    ConstV4F   _ -> TV4F
    ConstM22F  _ -> TM22F
    ConstM33F  _ -> TM33F
    ConstM44F  _ -> TM44F
    Stream t -> t.sType

class NumberStorable a where
  toArray :: a -> Array Number

instance intStorable  :: NumberStorable Int  where toArray a = [toNumber a]
instance numStorable  :: NumberStorable Number  where toArray a = [a]
instance boolStorable :: NumberStorable Boolean where toArray a = [if a then 1.0 else 0.0]
instance v2Storable   :: (NumberStorable a) => NumberStorable (V2 a)  where toArray (V2 x y) = concatMap toArray [x,y]
instance v3Storable   :: (NumberStorable a) => NumberStorable (V3 a)  where toArray (V3 x y z) = concatMap toArray [x,y,z]
instance v4Storable   :: (NumberStorable a) => NumberStorable (V4 a)  where toArray (V4 x y z w) = concatMap toArray [x,y,z,w]
instance arrStorable  :: (NumberStorable a) => NumberStorable (Array a)     where toArray a = concatMap toArray a

class IntStorable a where
  toIntArray :: a -> Array Int

instance intIntStorable  :: IntStorable Int  where toIntArray a = [a]
--instance numIntStorable  :: IntStorable Number  where toIntArray a = [a]
instance boolIntStorable :: IntStorable Boolean where toIntArray a = [if a then 1 else 0]
instance v2IntStorable   :: (IntStorable a) => IntStorable (V2 a)  where toIntArray (V2 x y) = concatMap toIntArray [x,y]
instance v3IntStorable   :: (IntStorable a) => IntStorable (V3 a)  where toIntArray (V3 x y z) = concatMap toIntArray [x,y,z]
instance v4IntStorable   :: (IntStorable a) => IntStorable (V4 a)  where toIntArray (V4 x y z w) = concatMap toIntArray [x,y,z,w]
instance arrIntStorable  :: (IntStorable a) => IntStorable (Array a)     where toIntArray a = concatMap toIntArray a

