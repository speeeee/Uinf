module Core.Hitbox (Hitbox(..), inHB, ptnCoords) where

import Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.GLFW as K

data Hitbox = Hitbox { xy :: (GLfloat,GLfloat),
                       xyn :: (GLfloat,GLfloat) } deriving (Show,Eq)

inHB :: (GLfloat,GLfloat) -> Hitbox -> Bool
inHB (x,y) hb =
  let ((xa,ya),(xb,yb)) = (xy hb, xyn hb) in
  x>=xa&&x<=xb&&y>=ya&&y<=yb

ptnCoords :: (Double,Double) -> K.Window -> IO (GLfloat,GLfloat)
ptnCoords (sx,sy) win = do
  (xc,yc) <- K.getCursorPos win
  (xf,yf) <- K.getFramebufferSize win
  let (x,y) = ((realToFrac xc)/1280,(realToFrac yc)/752)
  return (realToFrac (x*sx)::GLfloat,realToFrac (y*sy)::GLfloat)
