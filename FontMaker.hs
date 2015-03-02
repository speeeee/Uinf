module FontMaker (Character(..), Font(..), renderChar) where

import qualified Graphics.UI.GLFW as K
import Graphics.Rendering.OpenGL.Raw
import Data.List
import System.IO

data Font = Font { name :: String,
                   chars :: [Character] }
data Character = Character { key :: K.Key,
                             char :: [(Bool,(GLfloat,GLfloat))], --5x5
                             diacritic :: [(Bool,(GLfloat,GLfloat))] {-5x3-} }

renderChar :: Character -> (GLfloat,GLfloat) -> GLfloat -> IO ()
renderChar charac (xt,yt) sz = do
  renderList sz (map (\(x,y) -> (x+xt,y+yt)) (toCoords (char charac)))
  renderList sz (map (\(x,y) -> (x+xt,y+yt+5*sz)) (toCoords (diacritic charac)))

toCoords :: [(Bool,(GLfloat,GLfloat))] -> [(GLfloat,GLfloat)]
toCoords char = map snd (filter (\k -> fst k) char)

renderList :: GLfloat -> [(GLfloat,GLfloat)] -> IO ()
renderList sz lst = mapM_ (\(x,y) -> mksquare sz (x*sz,y*sz)) lst

mksquare :: GLfloat -> (GLfloat,GLfloat) -> IO ()
mksquare sz (x,y) = do
  glVertex3f x y 0
  glVertex3f (x+sz) y 0
  glVertex3f (x+sz) (y+sz) 0
  glVertex3f x (y+sz) 0
