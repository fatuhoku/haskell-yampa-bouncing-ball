module Bindings (keyboardMouse) where

import Graphics.UI.GLUT
import System.Exit

dPos = 10.0 :: GLfloat
{------------------------------------------------------------------------}
{- Miscellaneous Callbacks                                              -}
{------------------------------------------------------------------------}
{- Draws 4 really big 20pt 'points' on teach of the corners of a square -}
keyboardAct a p (Char '\27') Down = exitSuccess
keyboardAct a p (Char c) Down = do
  a' <- get a
  a $= case c of
         ' ' -> -a'
         '=' -> 2*a'
         '-' -> a'/2
         otherwise -> a'

keyboardAct a p (SpecialKey k) Down = do
  (x,y) <- get p
  p $= case k of
         KeyLeft   -> (x-dPos, y)
         KeyRight  -> (x+dPos, y)
         KeyUp     -> (x, y+dPos)
         KeyDown   -> (x, y-dPos)
         otherwise -> (x,y)

-- KeyboardAct is a simplified version of the keyboardMouse handler, so it seems
keyboardAct _ _ _ _ = return ()
keyboardMouse angle pos key state modifiers position = do
  keyboardAct angle pos key state

