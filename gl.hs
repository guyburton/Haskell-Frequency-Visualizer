--
--Code based loosely on the Nehe tutorials GHC port
--

module Main where

-- import Graphics.UI.GLUT
import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever, forM )
import Data.Time
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace

initGL :: IO ()
initGL = do
  glShadeModel gl_SMOOTH -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL -- type of depth test
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST

resizeScene :: GLFW.WindowSizeCallback
resizeScene w     0      = resizeScene w 1 -- prevent divide by zero
resizeScene width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100 
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

size :: GLfloat
size = 0.4

getBandMagnitude :: Int -> IORef [Double] -> IO(GLfloat)
getBandMagnitude index vals = do
    listOfVals <- readIORef vals
    return $ realToFrac $ listOfVals !! index

drawScene :: IORef Float -> IORef [Double] -> IO()
drawScene time bands = do
  t <- readIORef time
  
-- clear the screen and the depth bufer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view
  glTranslatef  (-5.0) (-4.0) (-15.0)
 
  forM [0 ..7] $ \i -> do
    glTranslatef 1.0 0.0 0.0
    glBegin gl_QUADS

    uncurryRGB glColor3f $ hsv (fromIntegral (i * 45)) 1.0 1.0

    mag <- getBandMagnitude i bands

    glVertex3f (-size) mag 0 -- top left
    glVertex3f ( size) mag 0 -- top right
    glVertex3f ( size) 0 0 -- bottom right
    glVertex3f (-size) 0 0 -- bottom left
   
    glVertex3f size mag 0    -- top left
    glVertex3f size mag size -- top right
    glVertex3f size 0 size -- bottom right
    glVertex3f size 0 0    -- bottom left
 
    glVertex3f (-size) mag 0    -- top left
    glVertex3f (-size) mag size -- top right
    glVertex3f (-size) 0 size -- bottom right
    glVertex3f (-size) 0 0    -- bottom left
    
    glVertex3f (-size) mag 0    -- top left
    glVertex3f (-size) mag size -- top right
    glVertex3f ( size) mag size -- bottom right
    glVertex3f ( size) mag 0    -- bottom left
  
    glVertex3f (-size) 0 0    -- top left
    glVertex3f (-size) 0 size -- top right
    glVertex3f ( size) 0 size -- bottom right
    glVertex3f ( size) 0 0    -- bottom left

    glVertex3f (-size) mag size -- top left
    glVertex3f ( size) mag size -- top right
    glVertex3f ( size) 0 size
    glVertex3f (-size) 0 size

    glEnd
  
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

keyPressed :: GLFW.KeyCallback 
keyPressed GLFW.KeyEsc True = shutdown >> return ()
keyPressed _           _    = return ()

main :: IO ()
main = do
     True <- GLFW.initialize 
     -- get a 640 x 480 window
     let dspOpts = GLFW.defaultDisplayOptions
                     { GLFW.displayOptions_width  = 640
                     , GLFW.displayOptions_height = 480
                     -- Set depth buffering and RGBA colors
                     , GLFW.displayOptions_numRedBits   = 8
                     , GLFW.displayOptions_numGreenBits = 8
                     , GLFW.displayOptions_numBlueBits  = 8
                     , GLFW.displayOptions_numAlphaBits = 8
                     , GLFW.displayOptions_numDepthBits = 1
                     -- , GLFW.displayOptions_displayMode  = GLFW.Fullscreen
                     } 
     time <- newIORef 0
     bands <- newIORef [0.0]

     writeIORef bands [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
     -- initialize our window.
     True <- GLFW.openWindow dspOpts
     -- window starts at upper left corner of the screen
     GLFW.setWindowPosition 0 0
     -- open a window
     GLFW.setWindowTitle "Guys FreqAnalyzer"
     -- register the function to do all our OpenGL drawing
     GLFW.setWindowRefreshCallback (drawScene time bands)
     -- register the funciton called when our window is resized
     GLFW.setWindowSizeCallback resizeScene
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback keyPressed
     -- register window close handler
     GLFW.setWindowCloseCallback shutdown
     initGL
     -- start event processing engine
     forever $ do
       oldBands <- readIORef bands

       t <- readIORef time
       writeIORef time $ t + 1
       t <- readIORef time

       writeIORef bands [(sin . realToFrac $ t * 0.002 * pi + i * (2 * pi / 8)  ) | i <- [0..7]] 

       drawScene time bands
       GLFW.swapBuffers
