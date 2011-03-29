module Main where

import System.Exit
import Data.Tuple.HT
import Data.IORef
import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL
import FRP.Yampa
import Graphics
import Types
import SF

-- Entry point.
main :: IO ()
main = do
  newInput <- newIORef NoEvent
  oldTime  <- newIORef (0 :: Int)
  rh <- reactInit (initGL >> return NoEvent) (\_ _ b -> b >> return False) mainSF
  -- We allow control over the position of the ball, though the rate of spin is
  -- determined as a constant.
  keyboardMouseCallback $= Just (keyMouse newInput)
  displayCallback       $= return ()
  idleCallback          $= Just (idle newInput oldTime rh)
  mainLoop
  where
    keyMouse _keyEv (Char '\27') Down _ _ = exitSuccess
    keyMouse keyEv  (Char ' ')   Down _ _ = writeIORef keyEv $ Event ()
    keyMouse _keyEv  _ _  _ _             = return ()

-- Update angle of the ball when idle.
-- angle       the angle should 
idle :: IORef (Event ())
     -> IORef Int
     -> ReactHandle (Event ()) (IO ())
     -> IO ()
idle newInput oldTime rh = do
  newInput' <- readIORef newInput
  newTime'  <- get elapsedTime
  oldTime'  <- readIORef oldTime
  let dt = let dt' = (fromIntegral $ newTime' - oldTime')/50
           in if dt' < 0.8 then dt' else 0.8                 -- clamping of time
  react rh (dt,Just newInput')
  writeIORef oldTime newTime'
  return ()
