module Circle (circle) where

import Data.Tuple.HT
import Graphics.Rendering.OpenGL

-- Draws a unit circle at (0,0). Defaults to drawing 24 fans.
circle r = let dAngle = pi/12 in do
  renderPrimitive TriangleFan $ do
    sequence_ $ map (vertex . uncurry3 Vertex3) $
      [ (r * (sin angle),r * (cos angle),0) | angle <- [0.0,dAngle..2*pi-0.0001] ]
