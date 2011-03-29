{-# LANGUAGE BangPatterns, Arrows #-}
module SF (mainSF) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics
import FRP.Yampa
import Control.Arrow
import Types

mainSF = pollInputs >>> update >>> draw

-- The boolean event says whether there has been a 'pat' on the ball or not.
-- Right now, I don't want to complicate this.
pollInputs :: SF (Event ()) (Event ())  
pollInputs = identity

-- I just want to process a spacebar!
-- These are ***pointwise*** bindings.
-- TODO Abstract out the initial state.
update :: SF (Event ()) SceneNode
update = proc e -> do
      ballSn   <- ballSF (50,150) 40 -< e
      groundSn <- groundSF 40 -< ()
      returnA  -< SnMulti [ballSn,groundSn]

gee :: (GLfloat, GLfloat)
gee = (0.0,-9.81)

-- TODO bounceSF - something that determines the amount of acceleration due to
-- the bounce

-- BallSF
-- 2D acceleration, 2D velocity, 2D position.
-- Given intial starting position, and a size and a constant radius
ballSF   :: (GLfloat,GLfloat) -> GLfloat -> SF (Event ()) SceneNode
ballSF pos rad = proc e -> do
  rec
      patAcc <- arr pat -< e
      let acc = pat e ^+^ patAcc
      vel    <- integral -< acc
      pos    <- integral' pos -< vel
  returnA -< SnBall (0.0) pos rad
  where
    pat :: (Event ()) -> (GLfloat,GLfloat)
    pat NoEvent = (0.0,0.0)
    pat (Event ()) = (0.0,-3.0)

groundSF :: GLfloat -> SF () SceneNode
groundSF top = arr $ const $ SnRect {
  rectColor = Color3 (0.0 :: GLfloat) (0.7) (0.0),
  rectTop = top
}

draw :: SF SceneNode (IO ())
draw = arr $ (\scnNode -> do
    clear [ColorBuffer]
    loadIdentity
    render scnNode
    swapBuffers)

-- Integral, taking intial value as first argument
integral' ini = (iPre zeroVector &&& time) >>> sscan f (ini, 0) >>> arr fst
    where
    f (!prevVal, !prevTime) (!val, !time) = 
      (prevVal ^+^ (realToFrac $ time - prevTime) *^ val, time)

