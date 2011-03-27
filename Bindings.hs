module Bindings (display,reshape,keyboardMouse) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Display

{------------------------------------------------------------------------}
{- Miscellaneous Callbacks                                              -}
{------------------------------------------------------------------------}
{- Draws 4 really big 20pt 'points' on teach of the corners of a square -}

reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

keyboardMouse key state modifiers position = return ()
