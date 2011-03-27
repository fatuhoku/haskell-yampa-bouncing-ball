module Main where

import Data.Tuple.HT
import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL
import Bindings

main = do
  initialDisplayMode $= [DoubleBuffered,RGBMode,WithDepthBuffer]
  (progName,_) <- getArgsAndInitialize
  createAWindow "Randy"
  mainLoop

createAWindow windowName = do
  createWindow windowName
  displayCallback $= display
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse


{------------------------------------------------------------------------}
{- Data                                                                 -}
{------------------------------------------------------------------------}
-- Let's draw a Square. Nice and easy, right?
-- There's 2 values to go between, let's say: 20 and 300
myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (x,y,0) | x <- [-0.75,0.75], y <- [0.75,-0.75] ]
