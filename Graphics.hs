module Graphics (initGL, render, reshape) where

import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Circle
import Cube
import Points
import Types
import FRP.Yampa
import FRP.Yampa.Vector3
import FRP.Yampa.Utilities

-- Ratio between width and height.
aspectRatio :: Size -> GLdouble
aspectRatio (Size w h)
  | h /= 0    = fromIntegral w / fromIntegral h
  | otherwise = error "Zero height"

winSize = Size 800 600

-- Set up viewport, projection, modelview matrices
-- Currently, we are using NDC directly. This is why we see that
-- when the window scales, the image scales too.
-- We must define an orthographic projection matrix for our window.
-- This effectively defines where our camera is, looking into the z = 0 plane
-- 
-- Let us therefore have a view of 800 units high, and 600 units wide. The
-- ground is at 50 units. The camera should be centred at 400 units, therefore.
-- Of course, we'll need a bigger ring, say, 77 units big
initGL :: IO ()
initGL = do
  (progName,_) <- getArgsAndInitialize
  initialDisplayMode    $= [DoubleBuffered,RGBMode]
  initialWindowSize     $= winSize
  createWindow "BallBounce!"
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

-- display :: IORef(GLDouble) -> IO ()
-- We might want to generalise for later:
--  scale
--  rotate
--  translate
--
--  scNode     the root scene node
-- We just use a list of scene nodes right now and modify the properties of the
-- transformation scene nodes to animate our scene.

spinRate = 0.1

-- Draw scene nodes.
render (SnBall a (x,y) rad) = preservingMatrix $ do
  translate $ Vector3 x y 0  -- transform the modelview matrix to move everything there.
  ringOfCircles 23 a rad -- default 'ball' representation
render (SnRect rectColour rectTop) = do
  color $ rectColour
  rect (Vertex2 (-400.0) (0.0 :: GLfloat)) (Vertex2 (400.0) (rectTop))
render (SnMulti nodes) = sequence_ $ map render nodes


-- Handler for when the window is reshaped
reshape s@(Size w h) = do
  viewport $= (Position 0 0, s) -- for carrying out digital zoom, kind of.
  postRedisplay Nothing


-- Draws circles of size 0.1 in a radius 
ringOfCircles n angle r = preservingMatrix $ do
    rotate angle $ Vector3 0 0 (1::GLfloat)
    let d = 2*r
    scale d d (d :: GLfloat)
    -- preservingMatrix wraps a pushMatrix and popMatrix call
    -- around the IO action given; effectively making any transformations
    -- within the body of the action self-contained.
    mapM_ (\(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
      translate $ Vector3 x y z
      circle (0.1::GLfloat)
      ) $ points n
