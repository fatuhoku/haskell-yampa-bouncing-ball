module Points where

import Graphics.Rendering.OpenGL

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n' = let n = fromIntegral n' in
            map (\k -> let t = 2*pi*k/n in (sin(t),cos(t),0.0))  [1..n]

