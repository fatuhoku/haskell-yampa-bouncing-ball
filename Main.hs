import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL
import Data.Tuple.HT

main = do
  getArgsAndInitialize
  createAWindow "red"
  mainLoop

createAWindow windowName = do
  createWindow windowName
  displayCallback $= display2

{- Draws 4 really big 20pt 'points' on teach of the corners of a square -}
display = do 
  pointSize $= 20
  currentColor $= Color4 1 0 0 1
  clearColor $= Color4 0 0 0 1
  clear [ColorBuffer]
  x <- get windowSize
  y <- get screenSize
  print (x,y)
  renderPrimitive Points $ -- Here we specify the render mode
    mapM_ (vertex . uncurry3 Vertex3) myPoints
  flush

{- Draws a teapot! -}
display2 = do
  clear [ColorBuffer]
  renderObject Solid $ Teapot 0.6
  flush

-- Let's draw a Square. Nice and easy, right?
-- There's 2 values to go between, let's say: 20 and 300
myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (x,y,0) | x <- [-0.75,0.75], y <- [0.75,-0.75] ]
