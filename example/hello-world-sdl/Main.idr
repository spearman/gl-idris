module Main

import Graphics.SDL2.SDL

import Control.Algebra
import Data.Matrix
import Data.Matrix.Algebraic

import System

import Graphics.Util.Transforms
import Graphics.Util.ObjLoader
import Graphics.Util.Mesh
import Graphics.Rendering.Gl
import Graphics.Rendering.Config

%include C "GL/glew.h"
%flag C "-Wno-pointer-sign"

||| contains the display / viewport dimension
record Display where
  constructor MkDisplay
  width : Int
  height : Int
  
||| returns the aspect ratio of a disply
aspectRatio : Display -> Double
aspectRatio (MkDisplay width height) = (cast width) / (cast height)

||| camera parameterers
||| the values in this data type will be used to calculate the view and perspective
||| projection matrices 
record Camera where
  constructor MkCamera
  ||| the 3D position of the camera
  position : Vec3
  ||| field of view of the camera
  fov : Angle
  ||| near plane limit of the camera frustum
  nearPlane : Double
  ||| far plane limit of the camera frustum
  farPlane : Double
  ||| the direction of the camera as a vector
  direction : Vec3

record ShaderLocations where
  constructor Locs
  viewMatrixLocation : Int
  
record State where
  constructor MkState
  renderer: SDLRenderer
  window: SDLWindow
  display : Display
  camera  : Camera
  locations: ShaderLocations
  entities : List (Entity String)

updateCameraPosition : State -> (Vec3 -> Vec3) -> State
updateCameraPosition state f = record { camera->position = f (record {camera->position} state) } state

update: Event -> State -> State
update (KeyDown (KeyAny 'w'))  state = updateCameraPosition state (\p => p <-> [0,    0,    0.05])
update (KeyDown (KeyAny 's'))  state = updateCameraPosition state (\p => p <+> [0,    0,    0.05])
update (KeyDown (KeyAny 'a'))  state = updateCameraPosition state (\p => p <-> [0.05, 0,    0])
update (KeyDown (KeyAny 'd'))  state = updateCameraPosition state (\p => p <+> [0.05, 0,    0])
update (KeyDown KeyUpArrow)    state = updateCameraPosition state (\p => p <+> [0,    0.05, 0])
update (KeyDown KeyDownArrow)  state = updateCameraPosition state (\p => p <-> [0,    0.05, 0])
update (KeyDown _)             state = state
update (KeyUp x)               state = state
update (MouseMotion x y dx dy) state = state
update (MouseButtonDown x y z) state = state
update (MouseButtonUp x y z)   state = state
update (MouseWheel x)          state = state
update (Resize x y)            state = state
update AppQuit                 state = state
update WindowEvent             state = state

setViewMatrix : State -> Entity String -> String -> IO ()
setViewMatrix state (SimpleEntity _ _ entityPosition rotation location _) s = do 
  let pos = entityPosition <-> (record { camera->position } state)
  let transform = (translate pos) <> (rotate rotation) <> (scaleAll 1)
  glUniformMatrix4fv location 1 0 (toList $ toGl transform)

draw : State -> IO ()
draw state = do 
                glClearColor 0.2 0.2 0.2 1
                glClear GL_COLOR_BUFFER_BIT
                glClear GL_DEPTH_BUFFER_BIT

                let locView = record {locations->viewMatrixLocation} state
                let cameraDirection = record {camera->direction} state
                glUniformMatrix4fv locView 1 0 (toList $ toGl $ standardViewMatrix cameraDirection)

                traverse (\entity => render entity (setViewMatrix state entity)) (entities state)
                   
                glSwapWindow $ window state
                   
camera : Camera
camera = MkCamera [0.0, 0.0, 0.0] (Degree 45) 0.1 100.0 [0.0, 0.0, -1.0]

display : Display
display = MkDisplay 800 600

main : IO ()
main = do 
          putStrLn "initializing SDL..."
          init <- initSDL
          if init /= 0 then do
            putStrLn "...SDL initialization failed"
            exit 1
          else do
            putStrLn "...SDL initialized"
            putStrLn "setting GL attributes..."

            glSetAttribute SDL_GL_CONTEXT_PROFILE_MASK (toSDLInt SDL_GL_CONTEXT_PROFILE_CORE)
            --glSetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 4
            --glSetAttribute SDL_GL_CONTEXT_MINOR_VERSION 1
            glSetAttribute SDL_GL_ACCELERATED_VISUAL 1
            glSetAttribute SDL_GL_DOUBLEBUFFER  1
            glSetAttribute SDL_GL_DEPTH_SIZE 16

            putStrLn "starting SDL..."
            (win,renderer) <- startSDL "Hello Idris" 800 600
            putStrLn "...SDL started"
            ctx <- createGLContext win
            glMakeCurrent win ctx
            
            glSetSwapInterval 1
            
            glewInit
            putStrLn "GLEW initialized..."

            info <- glGetInfo
            putStrLn info
            glEnable GL_DEPTH_TEST
            glDepthFunc GL_LESS
            
            texture <- loadTexture "stone.png" 0          
            (Right plane) <- loadObj "stone.obj" | (Left err) => do
              putStr "loadObj file error: "
              putStrLn $ show err
            planeModel <- createModel plane [texture]
            

            (Right shader_vert_src) <- readFile "shader.vert" | (Left err) => do
              putStr "error reading vert shader file:"
              putStrLn $ show err
            (Right shader_frag_src) <- readFile "shader.frag" | (Left err) => do
              putStr "error reading frag shader file:"
              putStrLn $ show err
            shader <- createShaders [(GL_VERTEX_SHADER, shader_vert_src), (GL_FRAGMENT_SHADER, shader_frag_src)]
            traverse printShaderLog  (shaders shader)
            glUseProgram $ program shader
            
            locView <- glGetUniformLocation (program shader) "viewMatrix"
            glUniformMatrix4fv locView 1 0 (toList $ toGl $ viewMatrix [0,0,-1] [0,0,0] [0,1,0])

            locProj <- glGetUniformLocation (program shader) "projectionMatrix"
            let projM = perspectiveProjection (fov camera) (aspectRatio display) ((nearPlane camera), (farPlane camera))
            glUniformMatrix4fv locProj 1 0 (toList $ toGl projM)

            loc <- glGetUniformLocation (program shader) "transformMatrix"          

            let entity = SimpleEntity planeModel shader [0,-0.5,-3] [(Degree 0),(Degree 0),(Degree 0)] loc "Test"
            
            let initialState = MkState renderer win display camera (Locs locView) [entity]
            eventLoop initialState
            deleteModel planeModel
            deleteShaders shader
            deleteTextures [texture]
            --deleteGLContext ctx
            endSDL win renderer
            pure ()
       where 
         eventLoop : State -> IO ()
         eventLoop state = do
                         e <- pollEvent
                         case e of
                           Just AppQuit => pure ()
                           Just event   => do 
                             draw state
                             --handle r event
                             --putStrLn $ "event" ++ (show event)
                             eventLoop $ update event state
                           _            => eventLoop state


