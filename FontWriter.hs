import qualified Graphics.UI.GLFW as K
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Bits ( (.|.) )
import System.Exit (exitWith, ExitCode(..))
import System.Environment
import Data.List

-- data VertDir = Up | Down
-- data HoriDir = Left | Right

(%) :: Int -> Int -> Int
(%) = rem
(//) :: Int -> Int -> Int
(//) = div
fInt :: (Integral a, Num b) => a -> b
fInt = fromIntegral

mkrect (x,y) (xa,ya) = do
  glVertex3f x y 0
  glVertex3f xa y 0
  glVertex3f xa ya 0
  glVertex3f x ya 0

drawGrid :: [Int] -> IO ()
drawGrid c = do
  mapM_ (\(n,k) -> do if k == 0 then glColor3f 1 1 1
                                else glColor3f 0 0 0
                      let (x,y) = ((fInt (n%5))*0.427,(fInt (4-n//5))*0.2+0.25)
                          (a,b) = ((fInt (if n%4==0&&n/=0 then 5 else (n+1)%5))*0.427,(fInt (5-n//5))*0.2+0.25)
                      mkrect (x+0.001,y+0.001) (a-0.001,b-0.001))
        (zip [(0::Int)..] c)

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

drawScene char _ = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity
  glTranslatef (-1.0675) (-0.625) (-1.5)
  glColor3f 0.4 0.4 0.4
  glBegin gl_QUADS
  mkrect (0,0) (2.135,1.25)
  drawGrid char
  glEnd

shutdown win = do
  K.destroyWindow win
  K.terminate
  _ <- exitWith ExitSuccess
  return ()

changeGrid :: K.Window -> (GLfloat,GLfloat) -> [Int] -> [Int]
changeGrid win c = do
  (xc,yc) <- K.getCursorPos win
  (xf,yf) <- K.getFramebufferSize win
  let (x,y) = ((realToFrac xc)/(realToFrac xf)*5,5-(realToFrac yc)/(realToFrac yf)*5)

getInput :: K.Window -> [Int] -> [Int]
getInput win char = do
  j <- K.getMouseButton win K.MouseButton'1
  if j == K.MouseButtonState'Pressed && K.getCursorPos win >= 0.25
  then changeGrid win (2.135,1.0) char else char

--keys :: K.Window -> K.KeyCallback -> FilePath -> IO ()
keys f _ k scan state mod = do
  when (state == K.KeyState'Pressed) (appendFile f ((show (fromEnum k)) ++ "\n"))

runGame win char = runGame' win char (0::Int)
runGame' win char acc = do
  K.pollEvents
  drawScene char win
  nchar <- getInput win char
  K.swapBuffers win
  runGame' win char (1 + acc)

main = do
  [c] <- getArgs
  True <- K.init
  Just win <- K.createWindow 1280 800 "Font Writer" Nothing Nothing
  K.makeContextCurrent (Just win)
  K.setWindowRefreshCallback win (Just (drawScene (take 25 [0,0..])))
  K.setFramebufferSizeCallback win (Just resizeScene)
  K.setWindowCloseCallback win (Just shutdown)
  K.setKeyCallback win (Just (keys c))
  initGL win
  runGame win (take 25 [0,0..])

