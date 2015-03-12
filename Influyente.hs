import qualified Graphics.UI.GLFW as K
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Bits ( (.|.) )
import System.Exit (exitWith, ExitCode(..))
import Control.Monad
import Data.List

import FontMaker
import Core.Hitbox

-- data VertDir = Up | Down
-- data HoriDir = Left | Right

data Mode = Main | Start | LoadGame | Fonts deriving (Show,Eq)
data Menu = Menu { buttons :: [(Int,Int)],
                   grids :: [Grid] }
data Grid = Grid { dim :: (Int,Int),
                   cellsz :: (GLfloat,GLfloat),
                   spacing :: GLfloat }

mkrect (x,y) (xa,ya) = do
  glVertex3f x y 0
  glVertex3f xa y 0
  glVertex3f xa ya 0
  glVertex3f x ya 0

initGL win = do
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  (w,h) <- K.getFramebufferSize win
  resizeScene win w h

resizeScene win w 0 = resizeScene win w 1
resizeScene win w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral w/fromIntegral h) 0.1 100
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

drawScene e mode _ = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity
  glTranslatef (-1.0675) (-0.625) (-1.5)
  glBegin gl_QUADS
  glColor3f 0.2 0.1 0.0
  mkrect (0,0) (2.135,1.25)
  glColor3f 0.3 0.2 0.0
  glVertex3f 0 0 0
  glVertex3f 0.5 0.0 0.0
  glVertex3f 0.5 1.25 0.0
  glVertex3f 0.0 1.25 0.0

  glColor3f 0.5 0.3 0.0
  glVertex3f 0.083 0.1 0.0
  glVertex3f 0.417 0.1 0.0
  glVertex3f 0.417 0.2 0.0
  glVertex3f 0.083 0.2 0.0
  glEnd

  when (mode == Fonts) (do glBegin gl_QUADS
                           glColor3f 0.15 0.1 0.0
                           mkrect (0.083,0.1) (0.417,0.2)
                           glColor3f 0.3 0.2 0.0
                           mkrect (0.55,0) (2.135,1.25)
                           glColor3f 1 1 1
                           renderText "spain" e (0.3,0.3) 0.05
                           glEnd)

shutdown win = do
  K.destroyWindow win
  K.terminate
  _ <- exitWith ExitSuccess
  return ()

isPressed :: K.KeyState -> Bool
isPressed K.KeyState'Pressed = True
isPressed K.KeyState'Repeating = True
isPressed _ = False

getInput :: K.Window -> Hitbox -> Mode -> IO Mode
getInput win hb mode = do
  (x,y) <- ptnCoords (2.135,1.25) win
  j <- K.getMouseButton win K.MouseButton'1
  --print ((realToFrac x::GLfloat),(realToFrac y::GLfloat))
  if j == K.MouseButtonState'Pressed && inHB (x,(1.25-y)) hb then return Fonts
  else return mode
  {-(do
    glBegin gl_QUADS
    glColor3f 0.1 0.0 0.0
    glVertex3f 0.083 0.1 0.0
    glVertex3f 0.417 0.1 0.0
    glVertex3f 0.417 0.2 0.0
    glVertex3f 0.083 0.2 0.0
    glEnd)-}


{-getInput :: K.Window -> IO (GLfloat, GLfloat)
getInput win = do
  x0 <- isPressed `fmap` K.getKey win K.Key'Left
  x1 <- isPressed `fmap` K.getKey win K.Key'Right
  y0 <- isPressed `fmap` K.getKey win K.Key'Down
  y1 <- isPressed `fmap` K.getKey win K.Key'Up
  let x0n = if x0 then -1 else 0
      x1n = if x1 then 1 else 0
      y0n = if y0 then -1 else 0
      y1n = if y1 then 1 else 0
  return (x0n + x1n, y0n + y1n)-}

runGame win e mode = runGame' win e mode (0::Int)
runGame' win e mode acc = do
  K.pollEvents
  drawScene e mode win
  nmode <- getInput win (Hitbox ((0.083::GLfloat), (0.1::GLfloat)) ((0.417::GLfloat), (0.2::GLfloat))) mode
  K.swapBuffers win
  runGame' win e nmode (1 + acc)

main = do
  True <- K.init
  Just win <- K.createWindow 1280 800 "Influyente" Nothing Nothing
  e <- readFont "alphabet.fnt"
  --let player = Player2D (0.5,0.4) (0,0) 0 False 0 0
  K.makeContextCurrent (Just win)
  K.setWindowRefreshCallback win (Just (drawScene e Main))
  K.setFramebufferSizeCallback win (Just resizeScene)
  K.setWindowCloseCallback win (Just shutdown)
  initGL win
  runGame win e Main

