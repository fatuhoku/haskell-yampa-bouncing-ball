module Bindings (keyboardMouse) where

import Data.IORef
import Graphics.UI.GLUT

dPos = 10.0 :: GLfloat
{------------------------------------------------------------------------}
{- Miscellaneous Callbacks                                              -}
{------------------------------------------------------------------------}
{- Draws 4 really big 20pt 'points' on teach of the corners of a square -}

keyboardAct _dAng pos (SpecialKey k) Down = do
  (x,y) <- get pos
  pos $=! case k of
         KeyLeft   -> (x-dPos, y)
         KeyRight  -> (x+dPos, y)
         KeyUp     -> (x, y+dPos)
         KeyDown   -> (x, y-dPos)
         otherwise -> (x,y)

-- KeyboardAct is a simplified version of the keyboardMouse handler, so it seems
keyboardAct _ _ _ _ = return ()
keyboardMouse angle pos key state modifiers position = do
  keyboardAct angle pos key state

-- Current very simple implementation
-- (\k ks m _ -> writeIORef $ Event ()) -- mark that an event occured
