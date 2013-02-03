module Simulate
(
simulateDisplay
)
where

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

import Data.IORef

import Physics.Falling.World.GenericWorld
import Physics.Falling3d.World3d
import WorldDraw


frequency :: Double
frequency = 60.0

updatePerSeconds :: Int
updatePerSeconds = round $ 1000.0 / frequency

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Smooth

   materialSpecular  Front $= Color4 1 1 1 1
   materialShininess Front $= 50
   diffuse (Light 0)       $= Color4 1 1 1 0

   lighting        $= Enabled
   light (Light 0) $= Enabled
   depthFunc       $= Just Less

display :: IORef (DefaultWorld3d Int) -> DisplayCallback
display worldRef = do
   clear [ ColorBuffer, DepthBuffer ]
   loadIdentity
   lookAt (Vertex3 2 5 10) (Vertex3 0 0 0) (Vector3 0 0.1 0)
   readIORef worldRef >>= drawWorld
   swapBuffers


update :: IORef (DefaultWorld3d Int) -> IO ()
update worldRef = do
                  modifyIORef worldRef (step 0.016)
                  postRedisplay Nothing
                  addTimerCallback updatePerSeconds $ (update worldRef)

reshape :: ReshapeCallback
reshape size = do
   viewport   $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   frustum (-1) 1 (-1) 1 1.5 20
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

simulateDisplay :: DefaultWorld3d Int -> IO ()
simulateDisplay initWorld = do
   worldRef <- newIORef initWorld
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode    $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize     $= Size 500 500
   initialWindowPosition $= Position 100 100
   _ <- createWindow progName
   myInit
   displayCallback       $= (display worldRef)
   reshapeCallback       $= Just reshape
   keyboardMouseCallback $= Just keyboard
   addTimerCallback updatePerSeconds (update worldRef)
   mainLoop
