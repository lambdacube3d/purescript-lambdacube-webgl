module Example1 where

import Prelude
import Data.List as List
import Graphics.WebGL as GL
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import LambdaCube.IR (AccumulationContext(..), Backend(..), BlendEquation(..), Blending(..), BlendingFactor(..), ClearImage(..), Command(..), ComparisonFunction(..), CullMode(..), FetchPrimitive(..), FragmentOperation(..), ImageRef(..), ImageSemantic(..), InputType(..), Parameter(..), Pipeline(..), PolygonMode(..), PolygonOffset(..), Program(..), ProvokingVertex(..), RasterContext(..), RenderTarget(..), Slot(..), TargetItem(..), Value(..))
import LambdaCube.LinearBase (V2(..), V4(..))
import LambdaCube.Mesh (Mesh(..), MeshAttribute(..), MeshPrimitive(..))
import LambdaCube.PipelineSchema (ObjectArraySchema(..), PipelineSchema(..), StreamType(..))
import LambdaCube.WebGL (uniformFloat, setScreenSize)
import LambdaCube.WebGL.Backend (allocPipeline, disposePipeline, renderPipeline, setPipelineInput)
import LambdaCube.WebGL.Input (mkWebGLPipelineInput, sortSlotObjects, uniformM44F)
import LambdaCube.WebGL.Mesh (addMesh, compileMesh)

--  Our vertices. Three consecutive floats give a 3D vertex; Three consecutive vertices give a triangle.
--  A cube has 6 faces with 2 triangles each, so this makes 6*2=12 triangles, and 12*3 vertices
g_vertex_buffer_data :: Array (V4 Number)
g_vertex_buffer_data =
    [ V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0  (-1.0) (-1.0) 1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0    1.0    1.0  1.0
    , V4   1.0  (-1.0)   1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0  (-1.0)   1.0  1.0
    , V4   1.0  (-1.0) (-1.0) 1.0
    , V4   1.0    1.0    1.0  1.0
    , V4 (-1.0) (-1.0)   1.0  1.0
    , V4   1.0  (-1.0)   1.0  1.0
    , V4   1.0    1.0    1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0) (-1.0)   1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4 (-1.0) (-1.0)   1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4   1.0    1.0    1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0    1.0    1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    ]

--  Two UV coordinatesfor each vertex. They were created with Blender.
g_uv_buffer_data :: Array (V2 Number)
g_uv_buffer_data =
    [ V2 0.0 0.0
    , V2 0.0 1.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 1.0 0.0
    , V2 0.0 0.0
    , V2 1.0 0.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 0.0 1.0
    , V2 1.0 0.0
    , V2 0.0 1.0
    , V2 1.0 1.0
    , V2 1.0 0.0
    , V2 0.0 0.0
    , V2 0.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 0.0 1.0
    , V2 0.0 0.0
    , V2 1.0 0.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 0.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 0.0 1.0
    , V2 0.0 0.0
    , V2 0.0 1.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 1.0 0.0
    ]

myCube :: Mesh
myCube = Mesh
    { mAttributes: fromFoldable
        [ Tuple "position4" (A_V4F g_vertex_buffer_data)
        , Tuple "vertexUV"  (A_V2F g_uv_buffer_data)
        ]
    , mPrimitive: P_Triangles
    }

samplePipeline :: Pipeline
samplePipeline = Pipeline
    { info: "samplepipeline"
    , backend: WebGL1
    , textures : []
    , samplers : []
    , targets : [ RenderTarget { renderTargets : [ TargetItem { targetSemantic: Color
                                                              , targetRef: Just (Framebuffer Color)
                                                              } ] }
                ]
    , programs : []
    , slots : []
    , commands :
      [ SetRenderTarget 0
      , ClearRenderTarget [ ClearImage { imageSemantic: Color
                                       , clearValue: VV4F (V4 0.0 0.0 0.4 1.0)
                                       } ]
      ]
    , streams: []
    }
    
gfx03Pipeline :: Pipeline
gfx03Pipeline = Pipeline
  { info: "gfx03pipeline"
  , backend: WebGL1
  , textures : []
  , samplers : []
  , targets : [ RenderTarget { renderTargets : [ TargetItem { targetSemantic: Depth
                                                            , targetRef: Just (Framebuffer Depth)
                                                            }
                                               , TargetItem { targetSemantic: Color
                                                            , targetRef: Just (Framebuffer Color)
                                                            } ]
                             }
              ]
  , programs: [ Program { programUniforms : fromFoldable [ Tuple "MVP2" M44F ]
                        , programStreams : fromFoldable [ Tuple "v" $ Parameter {name: "position" , ty: V3F } ]
                        , programInTextures : fromFoldable []
                        , programOutput : [ Parameter {name: "gl_FragColor" , ty: V4F } ]
                        , vertexShader :
                          """
                          #version 100
                          precision highp float;
                          uniform mat4 MVP2;
                          attribute vec3 v;
                          void main() {
                          gl_Position = ( MVP2 ) * (vec4( v ,1.0) );
                          gl_PointSize = 1.0;
                          }
                          """
                        , geometryShader : Nothing
                        , fragmentShader :
                          """
                          #version 100
                          precision highp float;
                          void main() {
                          gl_FragColor = vec4 ( 0.0,0.4,0.0,1.0 );
                          }
                          """
                        }
              , Program { programUniforms : fromFoldable [ Tuple "MVP2" M44F ]
                        , programStreams : fromFoldable [ Tuple "v" $ Parameter {name: "position" , ty: V3F } ]
                        , programInTextures : fromFoldable []
                        , programOutput : [ Parameter {name: "gl_FragColor" , ty: V4F } ]
                        , vertexShader :
                          """
                          #version 100
                          precision highp float;
                          uniform mat4 MVP2;
                          attribute vec3 v;
                          varying vec4 v0;
                          void main() {
                          v0 = vec4( v ,1.0);
                          gl_Position = ( MVP2 ) * ( vec4( v ,1.0) );
                          gl_PointSize = 1.0;
                          }
                          """
                        , geometryShader : Nothing
                        , fragmentShader :
                          """
                          #version 100
                          precision highp float;
                          varying vec4 v0;
                          void main() {
                          gl_FragColor = ( v0 ) + ( vec4 ( 1.0,1.4,1.0,0.6 ) );
                          }
                          """
                        }
              , Program { programUniforms : fromFoldable [ Tuple "MVP" M44F ]
                        , programStreams : fromFoldable [ Tuple "v" $ Parameter {name: "position4" , ty: V4F } ]
                        , programInTextures : fromFoldable []
                        , programOutput : [ Parameter {name: "gl_FragColor" , ty: V4F } ]
                        , vertexShader :
                          """
                          #version 100
                          precision highp float;
                          uniform mat4 MVP;
                          attribute vec4 v;
                          varying vec4 v0;
                          void main() {
                          v0 = v;
                          gl_Position = ( MVP ) * ( v );
                          gl_PointSize = 1.0;
                          }
                          """
                        , geometryShader : Nothing
                        , fragmentShader :
                          """
                          #version 100
                          precision highp float;
                          varying vec4 v0;
                          void main() {
                          gl_FragColor = ( v0 ) * ( vec4 ( 1.0,1.4,1.0,0.6 ) );
                          }
                          """
                        }
              ]
  , slots: [ Slot { slotName : "stream"
                  , slotUniforms : fromFoldable [ Tuple "MVP2" M44F ]
                  , slotStreams : fromFoldable [ Tuple "position" V3F ]
                  , slotPrimitive : Triangles
                  , slotPrograms : [ 0 , 1 ]
                  }
           , Slot { slotName : "stream4"
                  , slotUniforms : fromFoldable [ Tuple "MVP" M44F ]
                  , slotStreams : fromFoldable [ Tuple "position4" V4F ]
                  , slotPrimitive : Triangles
                  , slotPrograms : [ 2 ]
                  }
           ]
  , commands:
      [ SetRenderTarget 0
      , ClearRenderTarget [ ClearImage { imageSemantic: Depth
                                       , clearValue: VFloat 1000.0 }
                          , ClearImage { imageSemantic: Color
                                       , clearValue: VV4F (V4 0.5 0.0 0.4 1.0)
                                       }
          ]
      , SetRasterContext
          (TriangleCtx CullNone PolygonFill NoOffset LastVertex)
      , SetAccumulationContext $
        AccumulationContext { accViewportName : Nothing
                            , accOperations : List.fromFoldable
                              [ DepthOp Less false
                              , ColorOp
                                (Blend
                       { colorEqSrc: FuncAdd
                       , alphaEqSrc: FuncAdd
                       , colorFSrc: SrcAlpha
                       , colorFDst: OneMinusSrcAlpha
                       , alphaFSrc: SrcAlpha
                       , alphaFDst: OneMinusSrcAlpha
                       , color: (V4 1.0 1.0 1.0 1.0)
                       })
                                (VV4B (V4 true true true true))
                              ]
                            }
      , SetProgram 2
      , RenderSlot 1
      , SetRasterContext
          (TriangleCtx CullNone PolygonFill NoOffset FirstVertex)
      , SetAccumulationContext $ 
        AccumulationContext { accViewportName : Nothing
                            , accOperations : List.fromFoldable
                              [ DepthOp Less false
                              , ColorOp NoBlending (VV4B (V4 true true false false))
                              ]
                            }
      , SetProgram 1
      , RenderSlot 0
      , SetRasterContext
          (TriangleCtx CullNone (PolygonLine 20.0) NoOffset FirstVertex)
      , SetAccumulationContext $
            AccumulationContext { accViewportName : Nothing
                                , accOperations : List.fromFoldable
                                  [ DepthOp Always false
                                  , ColorOp NoBlending (VV4B (V4 true true false false))
                                  ]
                                }
      , SetProgram 0
      , RenderSlot 0
      ]
  , streams: []
  }

main :: Eff (console :: CONSOLE, exception :: EXCEPTION, ref :: REF) Unit
main = GL.runWebGL "glcanvas" (\s -> log s) \ context -> do
    let inputSchema = PipelineSchema
          { objectArrays : fromFoldable
            [ Tuple "stream"  $ ObjectArraySchema { primitive: Triangles
                                                  , attributes: fromFoldable [ Tuple "position"  Attribute_V3F
                                                                             , Tuple "normal" Attribute_V3F
                                                                             , Tuple "UVTex" Attribute_V2F
                                                                             ]}
            , Tuple "stream4" $ ObjectArraySchema { primitive: Triangles
                                                  , attributes: fromFoldable [ Tuple "position4" Attribute_V4F
                                                                             , Tuple "vertexUV" Attribute_V2F
                                                                             ]}
            ]
          , uniforms : fromFoldable [ Tuple "MVP" M44F
                                    , Tuple "MVP2" M44F
                                    ]
          }
    pplInput <- mkWebGLPipelineInput inputSchema
    w <- GL.getCanvasWidth context
    h <- GL.getCanvasHeight context
    setScreenSize pplInput (V2 w h)

    let mvp = V4 (V4 0.4692207 (-0.28573585) (-0.9593549) (-0.9574381)) (V4 0.0 2.395976 (-0.122928835) (-0.12268323)) (V4 (-1.719497) (-7.797232e-2) (-0.2617912) (-0.26126814)) (V4 0.0 0.0 3.8834958 4.0755367)
    uniformM44F "MVP" pplInput.uniformSetter mvp

    gpuCube <- compileMesh myCube

    _ <- addMesh pplInput "stream4" gpuCube []

    log "WebGL ready"
    ppl <- allocPipeline gfx03Pipeline -- samplePipeline
    log "Pipeline allocated"

    setPipelineInput ppl (Just pplInput)
    sortSlotObjects pplInput
    log "Setup pipeline input"

    renderPipeline ppl
    log "WebGL completed"
    disposePipeline ppl
    log "Pipeline disposed"
