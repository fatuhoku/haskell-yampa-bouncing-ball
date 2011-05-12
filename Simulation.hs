{-# LANGUAGE BangPatterns, Arrows #-}
module SF (simulate) where

import Graphics.Rendering.OpenGL hiding (Radius)
import Graphics.UI.GLUT hiding (Radius)
import Graphics
import FRP.Yampa
import Control.Arrow
import Types

type COR     = GLfloat
type Radius  = GLfloat
type Acc2    = (GLfloat,GLfloat)
type Vel2    = (GLfloat,GLfloat)
type Pos2    = (GLfloat,GLfloat)
type Dir2    = (GLfloat,GLfloat)

-- Top level signal function arrow
simulate = discardInputs >>> update >>> draw

data PhysicsContext = PhyC {
  gee :: Acc2,
  cor :: GLfloat -- coefficient of restitution
}

defPhysics = PhyC {
  gee = (0.0,-9.81),
  cor = 0.8
}

data Bounds = Bounds {
  xMin  :: GLfloat,
  yMin  :: GLfloat,
  xMax  :: GLfloat,
  yMax  :: GLfloat
}


-- The boolean event says whether there has been a 'pat' on the ball or not.
-- Right now, I don't want to complicate this.
discardInputs :: SF (Event ()) ()
discardInputs = arr $ const ()

-- I just want to process a spacebar!
-- These are ***pointwise*** bindings.
-- TODO Abstract out the initial state.
update :: SF () SceneNode
update = proc () -> do
  (pos,vel) <- bouncingBall (0,200) (40,45) (cor defPhysics) ballRadius bounds -< ()
  let ballSn = SnBall { ballAng = 0,  
                        ballPos = pos,
                        ballRad = ballRadius,
                        ballCol = ballColour }
  returnA  -< SnMulti [ballSn,groundSn]
  where
    bounds = Bounds { xMin = -400, yMin = 50, xMax = 400, yMax = 600 }
    ballRadius = 30
    ballColour = Color3 (0.7 :: GLfloat) (0.7) (0.7)
    groundSn = SnRect {
      rectColor = Color3 (0.0 :: GLfloat) (0.7) (0.0),
      rectTop = yMin bounds
    }

-- fallingBall pos0 vel0
-- ball with initial position and velocity.
fallingBall :: Pos2 -> Vel2 -> SF () (Pos2, Vel2)
fallingBall p0 v0 = proc () -> do
  v <- (v0 ^+^) ^<< integral -< gee defPhysics
  p <- (p0 ^+^) ^<< integral -< v
  returnA -< (p,v)

-- fallingBall' pos0 vel0 ground
-- This time around, we detect for collisions with the ground given by 'ground',
-- and record the position, velocity, and the direction normal to the 
-- at the location of the collision.
-- Carries out a check for each of the four bounds
fallingBall' :: Pos2 -> Vel2 -> Radius -> Bounds ->
                SF () ((Pos2, Vel2), Event (Dir2,(Pos2,Vel2)))
fallingBall' p0 v0 rad bounds = proc () -> do
  pv@(p,v) <- fallingBall p0 v0 -< ()
  hitXMin  <- edgeTag (1, 0) -< fst p <= xMin bounds + rad
  hitYMin  <- edgeTag (0, 1) -< snd p <= yMin bounds + rad
  hitXMax  <- edgeTag (-1,0) -< fst p >= xMax bounds - rad
  hitYMax  <- edgeTag (0,-1) -< snd p >= yMax bounds - rad
  let hitInfo = foldr1 (mergeBy mergeHits) [hitXMin,hitYMin,hitXMax,hitYMax]
  returnA -< (pv, hitInfo `attach` pv)
  where
    mergeHits = (^+^) -- simply add the two collision directions together.


-- We use the extra collision information as given by fallingBall' to compute
-- the new bouncing ball's velocity.
-- Bounces off all four sides of the screen!
bouncingBall :: Pos2 -> Vel2 -> COR -> Radius -> Bounds -> SF () (Pos2,Vel2)
bouncingBall p0 v0 cor rad bounds = bouncingBall' p0 v0
  where
    bouncingBall' p0 v0 =
      switch (fallingBall' p0 v0 rad bounds) $
        \(dir,(p,v)) -> bouncingBall' p (reflect dir ((-cor) *^ v))
    reflect l v = (2*(v `dot` l)/(l `dot` l)) *^ l ^-^ v
      

-- Renders the scene using OpenGL from the Render module
draw :: SF SceneNode (IO ())
draw = arr clearAndRender

-- Integral, taking intial value as first argument
integral' ini = (iPre zeroVector &&& time) >>> sscan f (ini, 0) >>> arr fst
    where
    f (!prevVal, !prevTime) (!val, !time) = 
      (prevVal ^+^ (realToFrac $ time - prevTime) *^ val, time)

