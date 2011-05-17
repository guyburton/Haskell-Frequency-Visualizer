--
--Code based loosely on the Nehe tutorials GHC port
-- 

module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.GLU.Raw ( gluPerspective )

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw
import Data.IORef
import Control.Monad

import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import Control.Concurrent.Thread
import System.Environment (getArgs)
import Sound.Pulse.Simple
import Data.WAVE
import Numeric.FFT

import Data.Complex
size :: GLfloat
size = 0.4

drawScene :: IORef [Double] -> IO()
drawScene bands = do
  -- clear the screen and the depth bufer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view
  glTranslatef  (-5.0) (-4.0) (-15.0)
  b <- readIORef bands
  let num = length b
  forM [0 .. (num-1)] $ \i -> do
    glTranslatef 1.0 0.0 0.0
    glBegin gl_QUADS

    -- Cycle through color spectrum for bars
    uncurryRGB glColor3f $ hsv (fromIntegral (i * (360 `div` num) )) 1.0 1.0
    -- Get magnitude for the current bar
    mag <- getBandMagnitude i bands

    -- Draw a cuboid for bar
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
  swapBuffers

resizeCallback size@(Size width height) = do
        glViewport 0 0 (fromIntegral width) (fromIntegral height)
        glMatrixMode gl_PROJECTION
        glLoadIdentity
        gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100 
        glMatrixMode gl_MODELVIEW
        glLoadIdentity
        glFlush

playChunk s window samplesRef bands = do
	samples <- readIORef samplesRef
	samplesPlayed <- playAudioChunk s bands $ samples
	writeIORef samplesRef $ drop samplesPlayed samples
	postRedisplay $ Just window

start :: String -> IO()
start input = do
     let progName = "Guys FreqAnalyzer"
     initialize progName []
     window <- createWindow progName
     windowSize $= (Size 640 480)
     actionOnWindowClose $= Exit

     bands <- newIORef [0.0]
     writeIORef bands [0..7]
     
     -- register the function to do all our OpenGL drawing
     displayCallback $= (drawScene bands)
     -- register the function called when our window is resized
     reshapeCallback $= Just resizeCallback

     glShadeModel gl_SMOOTH -- enables smooth color shading
     glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
     glClearColor 0 0 0 0 -- Clear the background color to black$
     glClearDepth 1 -- enables clearing of the depth buffer$
     glEnable gl_DEPTH_TEST
     glDepthFunc gl_LEQUAL -- type of depth test$
     
     s <- simpleNew Nothing "example" Play Nothing "FreqAnalyzer" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
     wave <- getWAVEFile input
     samplesRef <- newIORef $ getChannel 1 wave     
     idleCallback    $= Just (playChunk s window samplesRef bands)

     mainLoop

getBandMagnitude :: Int -> IORef [Double] -> IO(GLfloat)
getBandMagnitude index vals = do
    listOfVals <- readIORef vals
    return $ (*) 0.1 $ realToFrac $ listOfVals !! index

-- Gets a list of samples for the specified channel from the wave file
getChannel :: Int -> WAVE -> [Float]
getChannel channel wave = map (\x -> realToFrac . sampleToDouble $ x !! channel) $ waveSamples wave

-- performs fft on provided sample buffer and returns a magnitude values
doFFT :: [Float] -> [Double]
doFFT samples = map magnitude . fft $ map (\x -> realToFrac x :+ 0) $ samples

-- folds the fft output into a number of bins
foldFFT :: Int -> [Double] -> [Double]
foldFFT 0 _ = []
foldFFT bins binMags = 
    [sum $ take num binMags] ++ foldFFT (num-1) (drop num binMags)
    where 
        num = length binMags `div` bins

-- Recursively play complete track in sample chunks
-- Work out the FFT of the first section and write to IO ref
playAudioChunk :: Simple -> IORef [Double] -> [Float] -> IO(Int)
playAudioChunk _ _ [] = return 0
playAudioChunk s bands samples = do
    let buffer = take 8192 samples
    simpleWrite s buffer
    writeIORef bands $ foldFFT 8 $ doFFT $ take 256 samples
    return 8192    

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input] -> do
            start input
        _ -> putStrLn "Must specify filename"


