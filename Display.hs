module Display (display) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Cube
import Points

display = do 
  clear [ColorBuffer]
  scale 0.7 0.7 (0.7 :: GLfloat)
  mapM_ (\(x,y,z) -> preservingMatrix $ do
    color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
    translate $ Vector3 x y z
    cube (0.1::GLfloat)
    ) $ points 7
  flush
