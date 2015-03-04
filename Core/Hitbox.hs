module Core.Hitbox (Hitbox(..), inHB) where

import Graphics.Rendering.OpenGL.Raw

data Hitbox = Hitbox { xy :: (GLfloat,GLfloat),
                       xyn :: (GLfloat,GLfloat) } deriving (Show,Eq)

inHB :: (GLfloat,GLfloat) -> Hitbox -> Bool
inHB (x,y) hb =
  let ((xa,ya),(xb,yb)) = (xy hb, xyn hb) in
  x>=xa&&x<=xb&&y>=ya&&y<=yb
