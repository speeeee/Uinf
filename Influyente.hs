import qualified Graphics.UI.GLFW as K
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Bits ( (.|.) )
import System.Exit (exitWith, ExitCode(..))
import Data.List

import FontMaker

-- data VertDir = Up | Down
-- data HoriDir = Left | Right

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

drawScene _ = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity
  glTranslatef 0 0 (-1.5)
  glBegin gl_QUADS
  glColor3f 0.0 0.5 0.7
  glVertex3f 0 0 0.0
  glVertex3f (0 + 0.1) 0 0.0
  glVertex3f (0 + 0.1) (0 + 0.1) 0.0
  glVertex3f 0 (0 + 0.1) 0.0

  glColor3f 0.9 0.8 0.0
  glVertex3f 0.2 (-0.2) 0.0
  glVertex3f 0.6 (-0.2) 0.0
  glVertex3f 0.6 0.2 0.0
  glVertex3f 0.2 0.2 0.0

  glVertex3f 0.6 (-0.2) 0.0
  glVertex3f 1.0 (-0.2) 0.0
  glVertex3f 1.0 0.6 0.0
  glVertex3f 0.6 0.6 0.0
  glEnd

shutdown win = do
  K.destroyWindow win
  K.terminate
  _ <- exitWith ExitSuccess
  return ()

isPressed :: K.KeyState -> Bool
isPressed K.KeyState'Pressed = True
isPressed K.KeyState'Repeating = True
isPressed _ = False

getInput :: K.Window -> IO (GLfloat, GLfloat)
getInput win = do
  x0 <- isPressed `fmap` K.getKey win K.Key'Left
  x1 <- isPressed `fmap` K.getKey win K.Key'Right
  y0 <- isPressed `fmap` K.getKey win K.Key'Down
  y1 <- isPressed `fmap` K.getKey win K.Key'Up
  let x0n = if x0 then -1 else 0
      x1n = if x1 then 1 else 0
      y0n = if y0 then -1 else 0
      y1n = if y1 then 1 else 0
  return (x0n + x1n, y0n + y1n)

runGame win = runGame' win 0
runGame' win acc = do
  K.pollEvents
  drawScene win
  K.swapBuffers win
  runGame' win (1 + acc)

main = do
  True <- K.init
  Just win <- K.createWindow 1280 800 "Influyente" Nothing Nothing
  --let player = Player2D (0.5,0.4) (0,0) 0 False 0 0
  K.makeContextCurrent (Just win)
  K.setWindowRefreshCallback win (Just (drawScene))
  K.setFramebufferSizeCallback win (Just resizeScene)
  K.setWindowCloseCallback win (Just shutdown)
  initGL win
  runGame win

