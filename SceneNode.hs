module SceneNode (SceneNode(..),
        ballAngle, ballPos,
        rectColor, rectTop
        ) where

import Data.IORef
import Graphics.Rendering.OpenGL

-- We have two kinds of scene node: ball and ground.
data SceneNode
  = SnBall {
        ballAngle :: IORef(GLfloat),
        ballPos   :: IORef(GLfloat,GLfloat)
    }
  | SnRect {
        rectColor :: Color3 GLfloat,
        rectTop   :: GLfloat -- the top edge of the rectangle
    }
  | SnMulti [SceneNode] -- List of scenenodes
