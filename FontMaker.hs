module FontMaker (Character(..), Font(..), renderChar, renderText, readFont) where

import qualified Graphics.UI.GLFW as K
import Graphics.Rendering.OpenGL.Raw
import Data.List
import Data.Char
import Control.Monad
import System.IO

data Font = Font { name :: String,
                   chars :: [Character] } deriving (Show,Eq)
data Character = Character { key :: Char,
                             char :: [(Bool,(GLfloat,GLfloat))], --5x5
                             diacritic :: [(Bool,(GLfloat,GLfloat))] {-5x3-} } deriving (Show,Eq)

renderText :: String -> [Character] -> (GLfloat,GLfloat) -> GLfloat -> IO ()
renderText s f (x,y) sz =
  --let ch = findChr (k-97) f
  mapM_ (\(n,k) -> case findChr k f of Just ch -> renderChar ch (x+(n+n/10)*sz*5,y) sz
                                       Nothing -> print "Char does not exist!") (zip [0..] (map ord s))

findChr :: Int -> [Character] -> Maybe Character
findChr n f =
  let c = chr n in
  find (\k -> key k == c) f

renderChar :: Character -> (GLfloat,GLfloat) -> GLfloat -> IO ()
renderChar charac pt sz = do
  renderList sz pt (toCoords (char charac))
  --renderList sz (map (\(x,y) -> (x+xt,y+yt+5*sz)) (toCoords (diacritic charac)))

toCoords :: [(Bool,(GLfloat,GLfloat))] -> [(GLfloat,GLfloat)]
toCoords char = map snd (filter (\k -> fst k) char)

renderList :: GLfloat -> (GLfloat,GLfloat) -> [(GLfloat,GLfloat)] -> IO ()
renderList sz pt lst = mapM_ (\(x,y) -> mksquare sz pt (x,y)) lst

mksquare :: GLfloat -> (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> IO ()
mksquare sz (xt,yt) (xi,yi) = do
  let (x,y) = (xt+sz*xi,yt+(5*sz-sz*yi))
  glVertex3f x y 0
  glVertex3f (x+sz) y 0
  glVertex3f (x+sz) (y-sz) 0
  glVertex3f x (y-sz) 0

readFont :: FilePath -> IO [Character]
readFont f = do
  l <- readFile f
  return (fontMake (lines l) [])

fontMake :: [String] -> [Character] -> [Character]
fontMake [] char = char
fontMake lns char = fontMake (drop 6 lns) (char ++ [(mkchar (take 6 lns))])

mkchar :: [String] -> Character
mkchar c =
  Character (chr ((read (head c)::Int)+97))
            (concat (map (\(y,k) -> map (\(x,q) -> if q == 't' then (True,(x,y))
                                                               else (False,(x,y))) (zip [(0::GLfloat)..] k)) (zip [(0::GLfloat)..] (tail c)))) []


