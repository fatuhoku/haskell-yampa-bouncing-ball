module Main where

import Data.Tuple.HT
import Data.IORef
import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL
import Bindings
import Display
import SceneNode

-- Let us draw a 
main = do
  initialDisplayMode $= [DoubleBuffered,RGBMode,WithDepthBuffer]
  (progName,_) <- getArgsAndInitialize
  createAWindow "Randy"
  mainLoop

-- Set up viewport, projection, modelview matrices
-- Currently, we are using NDC directly. This is why we see that
-- when the window scales, the image scales too.
-- We must define an orthographic projection matrix for our window.
-- This effectively defines where our camera is, looking into the z = 0 plane
-- 
-- Let us therefore have a view of 800 units high, and 600 units wide. The
-- ground is at 50 units. The camera should be centred at 400 units, therefore.
-- Of course, we'll need a bigger ring, say, 77 units big
createAWindow windowName = do
  -- initialDisplayMode $= [...]
  initialWindowSize     $= winSize
  createWindow windowName
  clear [ColorBuffer]
  viewport              $= ((Position 0 0),winSize)
  matrixMode            $= Projection
  loadIdentity
  let asp = aspectRatio winSize
  -- To retain aspect ratio in orthographic projection, we make the width
  -- dependent on the height, and the asp.
  ortho2D (-400) (400) (0.0) (600.0)
  matrixMode            $= Modelview 0
  loadIdentity
  reshapeCallback       $= Just reshape
  
  -- Initialise IORefs for the objects in our scene...!
  -- I guess we need to vary these using the idle function.
  pos  <- newIORef (0.0 :: GLfloat, 150.0)
  dAng <- newIORef (0.1 :: GLfloat)   -- the rate at which our ball should spin
  ang  <- newIORef (0.0 :: GLfloat) -- the orientation of our ball

  -- Initialise the scene nodes of the game, and create something
  let ballNode   = SnBall { ballAngle = ang, ballPos = pos }
  let groundNode = SnRect { rectColor = Color3 (0 :: GLfloat) 0.5 0, rectTop = 40 }
  let rootNode   = SnMulti [ballNode, groundNode]

  -- We allow control over the position of the ball, though the rate of spin is
  -- determined as a constant.
  keyboardMouseCallback $= Just (keyboardMouse dAng pos)
  displayCallback       $= (display rootNode)
  idleCallback          $= Just (idle dAng ang)

-- Ratio between width and height.
aspectRatio :: Size -> GLdouble
aspectRatio (Size w h)
  | h /= 0    = fromIntegral w / fromIntegral h
  | otherwise = error "Zero height"

winSize = Size 800 600

{------------------------------------------------------------------------}
{- Data                                                                 -}
{------------------------------------------------------------------------}
-- Let's draw a Square. Nice and easy, right?
-- There's 2 values to go between, let's say: 20 and 300
-- ***** NOTHING HERE *****
