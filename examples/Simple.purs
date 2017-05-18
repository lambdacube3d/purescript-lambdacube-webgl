module Simple where

import Prelude
import Data.List as List
import Graphics.WebGL as GL
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, fromFoldable)
import Data.Tuple (Tuple(..))
import LambdaCube.IR (AccumulationContext(..), Backend(..), Blending(..), ClearImage(..), Command(..), ComparisonFunction(..), CullMode(..), FetchPrimitive(..), FragmentOperation(..), ImageRef(..), ImageSemantic(..), InputType(..), Parameter(..), Pipeline(..), PolygonMode(..), PolygonOffset(..), Program(..), ProvokingVertex(..), RasterContext(..), RenderTarget(..), Slot(..), TargetItem(..), Value(..))
import LambdaCube.LinearBase (V2(..), V4(..))
import LambdaCube.Mesh (Mesh(..), MeshAttribute(..), MeshPrimitive(..))
import LambdaCube.PipelineSchema (ObjectArraySchema(..), PipelineSchema(..), StreamType(..))
import LambdaCube.WebGL (setScreenSize)
import LambdaCube.WebGL.Backend (allocPipeline, disposePipeline, renderPipeline, setPipelineInput)
import LambdaCube.WebGL.Input (mkWebGLPipelineInput, sortSlotObjects)
import LambdaCube.WebGL.Mesh (addMesh, compileMesh)

quad :: Mesh
quad = Mesh
  { mAttributes: fromFoldable
      [ Tuple "position" $ A_V2F [ V2 n p
                                 , V2 n n
                                 , V2 p n
                                 , V2 p n
                                 , V2 p p
                                 , V2 n p
                                 ]
      ]
  , mPrimitive: P_Triangles
  }
  where p = 0.5
        n = -p

simplePipeline :: Pipeline
simplePipeline = Pipeline
  { info : "simple"
  , backend : WebGL1
  , textures : []
  , samplers : []
  , targets :
      [ RenderTarget
          { renderTargets :
              [ TargetItem { targetSemantic : Color
                           , targetRef : Just (Framebuffer Color)
                           }
              ]
          }
      ]
  , programs :
      [ Program
          { programUniforms : empty
          , programStreams : fromFoldable [ Tuple "p" $ Parameter { name : "position"
                                                                  , ty : V2F
                                                                  } ]
          , programInTextures : empty
          , programOutput : [ Parameter { name : "gl_FragColor"
                                        , ty : V4F
                                        } ]
          , vertexShader :
              """
              attribute vec2 p;
              void main() {
                  gl_Position = vec4(p.x, p.y, 0.0, 1.0);
              }
              """
          , geometryShader : Nothing
          , fragmentShader :
              """
              void main() {
                  gl_FragColor = vec4(1.0,0.0,0.0,1.0);
              }
              """
          }
      ]
  , slots :
      [ Slot
          { slotName : "stream"
          , slotStreams : fromFoldable [ Tuple "position" V2F ]
          , slotUniforms : empty
          , slotPrimitive : Triangles
          , slotPrograms : [ 0 ]
          }
      ]
  , streams : []
  , commands :
      [ SetRenderTarget 0
      , ClearRenderTarget
          [ ClearImage { imageSemantic : Color
                       , clearValue : VV4F (V4 0.0 0.0 1.0 1.0)
                       }
          ]
      , SetProgram 0
      , SetRasterContext
          (TriangleCtx CullNone PolygonFill NoOffset LastVertex)
      , SetAccumulationContext $
          AccumulationContext
            { accViewportName : Nothing
            , accOperations : List.fromFoldable
                [ ColorOp NoBlending (VV4B (V4 true true true true)) ]
            }
      , RenderSlot 0
      ]
  }

main :: Eff (console :: CONSOLE, exception :: EXCEPTION, ref :: REF) Unit
main = GL.runWebGL "glcanvas" (\s -> log s) \ ctx -> do
  pplInput <- mkWebGLPipelineInput
              $ PipelineSchema { objectArrays : fromFoldable
                                 [ Tuple "stream"
                                   $ ObjectArraySchema { primitive: Triangles
                                                       , attributes: fromFoldable [ Tuple "position" Attribute_V2F ]
                                                       }
                                 ]
                               , uniforms : empty
                               }
  w <- GL.getCanvasWidth ctx
  h <- GL.getCanvasHeight ctx
  setScreenSize pplInput (V2 w h)
  mesh <- compileMesh quad
  _ <- addMesh pplInput "stream" mesh []
  sortSlotObjects pplInput
  ppl <- allocPipeline simplePipeline
  setPipelineInput ppl (Just pplInput)
  renderPipeline ppl
  disposePipeline ppl
  log "Done"
  
