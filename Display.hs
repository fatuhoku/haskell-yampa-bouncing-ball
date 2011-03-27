module Display (display,idle,reshape) where

import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Circle
import Cube
import Points
import SceneNode

-- display :: IORef(GLDouble) -> IO ()
-- We might want to generalise for later:
--  scale
--  rotate
--  translate
--
--  scNode     the root scene node
-- We just use a list of scene nodes right now and modify the properties of the
-- transformation scene nodes to animate our scene.
display :: SceneNode -> IO ()
display scnNode = do 
  clear [ColorBuffer]
  loadIdentity
  draw scnNode
  swapBuffers

spinRate = 0.1

-- Draw scene nodes.
draw (SnBall ang pos) = preservingMatrix $ do
  (x,y) <- get pos      -- this thing is modified by the simulation.
  a     <- get ang      -- retrieve the angle
  translate $ Vector3 x y 0  -- transform the modelview matrix to move everything there.
  ringOfCircles 23 a 40 -- default 'ball' representation
draw (SnRect rectColour rectTop) = do
  color $ rectColour
  rect (Vertex2 (-400.0) (0.0 :: GLfloat)) (Vertex2 (400.0) (rectTop))
draw (SnMulti nodes) = sequence_ $ map draw nodes

reshape s@(Size w h) = do
  viewport $= (Position 0 0, s) -- for carrying out digital zoom, kind of.
  postRedisplay Nothing

-- Update angle of the ball when idle.
-- angle       the angle should 
idle :: IORef(GLfloat) -> IORef(GLfloat) -> IO ()
idle dAngle angle = do
  a <- get angle
  d <- get dAngle
  angle $=! (a + d)
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
