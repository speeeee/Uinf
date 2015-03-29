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
import Core.Nations
import Util.Map

-- data VertDir = Up | Down
-- data HoriDir = Left | Right

data Mode = Main | GameBegin | Start | LoadGame | Fonts deriving (Show,Eq)
data Game = Game { nation :: Region,
                   buttons :: [(GLfloat,GLfloat)] }
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

drawScene e mode nat _ = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity
  glTranslatef (-1.0675) (-0.625) (-1.5)
  glBegin gl_QUADS
  glColor3f 0.2 0.1 0.0
  mkrect (0,0) (2.135,1.25)
  glColor3f 0.3 0.2 0.0
  mkrect (0,0) (0.5,1.25)

  glColor3f 0.5 0.3 0.0
  mkrect (0.083,0.1) (0.413,0.2)
  mkrect (0.083,1.05) (0.413,1.15)
  glColor3f 1 1 1
  renderText "font" e (0.175,0.14) 0.005
  renderText "start" e (0.175,1.09) 0.005
  glEnd

  case mode of
       Fonts -> do glBegin gl_QUADS
                   glColor3f 0.15 0.1 0.0
                   mkrect (0.083,0.1) (0.417,0.2)
                   glColor3f 0.3 0.2 0.0
                   mkrect (0.55,0) (2.135,1.25)
                   glColor3f 1 1 1
                   renderText "magyarorszag vagyok" e (0.3,0.3) 0.005
                   glEnd
       Start -> do glBegin gl_QUADS
                   glColor3f 0.15 0.1 0.0
                   mkrect (0.083,1.05) (0.417,1.15)
                   glColor3f 0.3 0.2 0.0
                   mkrect (0.55,0) (2.135,1.25)
                   glColor3f 1 1 1
                   mapM_ (\(y,k) -> renderText k e (0.6,y) 0.0045) (zip [1.22,1.175..0] (map name nations))
                   glEnd
       _ -> do glBegin gl_QUADS
               renderText "title screen" e (0.7,1.0) 0.005
               glEnd

drawMap m = do
  let xb = 2.135/32
      yb = 1.25/24
  mapM_ (\(y,k) -> mapM_ (\(x,q) -> do if q/=0 then glColor3f 1 1 1 else glColor3f 0 0 0
                                       mkrect (x*xb,y*yb) ((x+1)*xb,(y+1)*yb)) (zip [0..] k)) (zip [23,22..] m)

drawBoard e nat _ = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity
  glTranslatef (-1.0675) (-0.625) (-1.5)
  glBegin gl_QUADS
  glColor3f 1 1 1
  drawMap wmap
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

getInput :: K.Window -> [Hitbox] -> Mode -> Region -> IO (Mode,Region)
getInput win hb mode nat = do
  (x,y) <- ptnCoords (2.135,1.25) win
  j <- K.getMouseButton win K.MouseButton'1
  if j == K.MouseButtonState'Pressed && inHB (x,(1.25-y)) (hb!!0) then return (Fonts,nat)
  else if j == K.MouseButtonState'Pressed && inHB (x,(1.25-y)) (hb!!1) then return (Start,nat)
  else if mode == Start && x >= 0.6 && y <= 0.855 && j == K.MouseButtonState'Pressed
       then return (GameBegin, nations!!(truncate (y/0.855*19)))
       else return (mode,nat)


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

playGame :: K.Window -> [Character] -> Region -> IO ()
playGame win e nat = playGame' win e nat (0::Int)
playGame' win e nat acc = do
  K.pollEvents
  drawBoard e nat win
  K.swapBuffers win
  playGame' win e nat (acc + 1)

runGame win e mode nat = runGame' win e mode nat (0::Int)
runGame' win e mode nat acc = do
  K.pollEvents
  drawScene e mode nat win
  (nmode,nnat) <- getInput win [Hitbox ((0.083::GLfloat),0.1) ((0.417::GLfloat),0.2),
                                Hitbox ((0.083::GLfloat),1.05) ((0.417::GLfloat), 1.15)] mode nat
  K.swapBuffers win
  if nmode == GameBegin then playGame win e nnat else runGame' win e nmode nnat (1 + acc)

main = do
  True <- K.init
  Just win <- K.createWindow 1280 800 "Influyente" Nothing Nothing
  e <- readFont "alphabet.fnt"
  --let player = Player2D (0.5,0.4) (0,0) 0 False 0 0
  K.makeContextCurrent (Just win)
  K.setWindowRefreshCallback win (Just (drawScene e Main (Region "none" [] 0 0 [] [])))
  K.setFramebufferSizeCallback win (Just resizeScene)
  K.setWindowCloseCallback win (Just shutdown)
  initGL win
  runGame win e Main (Region "none" [] 0 0 [] [])

