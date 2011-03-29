module Types (
  SceneNode(..),
  KeyboardInput(..)
) where

import FRP.Yampa
import FRP.Yampa.Vector3
import Data.IORef
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

data KeyboardInput = KeyboardInput {
  key       :: Key,
  keyState  :: KeyState,
  modifiers :: Modifiers
}

-- SnRect is used to draw the ground!
data SceneNode
  = SnBall {
        ballAngle :: GLfloat,
        ballPos   :: (GLfloat,GLfloat),
        ballRad   :: GLfloat
    }
  | SnRect {
        rectColor :: Color3 GLfloat,
        rectTop   :: GLfloat -- the top edge of the rectangle
    }
  | SnMulti [SceneNode] -- List of scenenodes
