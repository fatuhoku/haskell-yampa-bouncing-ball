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
type Ground  = GLfloat
type Acc2    = (GLfloat,GLfloat)
type Vel2    = (GLfloat,GLfloat)
type Pos2    = (GLfloat,GLfloat)

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

-- The boolean event says whether there has been a 'pat' on the ball or not.
-- Right now, I don't want to complicate this.
discardInputs :: SF (Event ()) ()
discardInputs = arr $ const ()

-- I just want to process a spacebar!
-- These are ***pointwise*** bindings.
-- TODO Abstract out the initial state.
update :: SF () SceneNode
update = proc () -> do
  (pos,vel) <- bouncingBall (0,200) zeroVector (cor defPhysics) ballRadius ground -< ()
  let ballSn = SnBall { ballAng = 0,  
                        ballPos = pos,
                        ballRad = ballRadius,
                        ballCol = ballColour }
  returnA  -< SnMulti [ballSn,groundSn]
  where
    ballRadius = 30
    ballColour = Color3 (0.7 :: GLfloat) (0.7) (0.7)
    ground = 40
    groundSn = SnRect {
      rectColor = Color3 (0.0 :: GLfloat) (0.7) (0.0),
      rectTop = ground
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
-- and record the position and velocity at the location of the collision.
fallingBall' :: Pos2 -> Vel2 -> Radius -> Ground ->
                SF () ((Pos2, Vel2), Event (Pos2,Vel2))
fallingBall' p0 v0 rad ground = proc () -> do
  pv@(p,_) <- fallingBall p0 v0 -< ()
  hit      <- edge              -< snd p <= ground + rad
  returnA -< (pv, hit `tag` pv)


-- We use the extra collision information as given by fallingBall' to compute
-- the new bouncing ball's velocity.
bouncingBall :: Pos2 -> Vel2 -> COR -> Radius -> Ground -> SF () (Pos2,Vel2)
bouncingBall p0 v0 cor rad ground = bouncingBall' p0 v0
  where
    bouncingBall' p0 v0 =
      switch (fallingBall' p0 v0 rad ground) $ \(p,v) -> bouncingBall' p ((-cor)*^v)
      

-- Renders the scene using OpenGL from the Render module
draw :: SF SceneNode (IO ())
draw = arr clearAndRender

-- Integral, taking intial value as first argument
integral' ini = (iPre zeroVector &&& time) >>> sscan f (ini, 0) >>> arr fst
    where
    f (!prevVal, !prevTime) (!val, !time) = 
      (prevVal ^+^ (realToFrac $ time - prevTime) *^ val, time)

