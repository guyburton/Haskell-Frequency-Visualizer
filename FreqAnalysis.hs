
module GuyAudio where

import Data.Complex
import Numeric.FFT
import Data.WAVE
import System.Environment (getArgs)
import Control.Monad
import Data.IORef
import Sound.Pulse.Simple

-- Gets a list of samples for the specified channel from the wave file
getChannel :: Int -> WAVE -> [Float]
getChannel channel wave = map (\x -> realToFrac . sampleToDouble $ x !! channel) $ waveSamples wave

doFFT :: [Float] -> [Double]
doFFT samples = map magnitude . fft $ map (\x -> realToFrac x :+ 0) $ samples

foldFFT :: Int -> [Double] -> [Double]
foldFFT 0 _ = []
foldFFT bins binMags = 
    [sum $ take num binMags] ++ foldFFT (num-1) (drop num binMags)
    where 
        num = length binMags `div` bins

playAudio :: String -> IORef [Double] -> IO()
playAudio file bands = do
    s <- simpleNew Nothing "example" Play Nothing "this is an example application" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    wave <- getWAVEFile file
    playAudio2 s bands $ getChannel 1 wave
    simpleFree s

-- Recursively play complete track in 4096 sample chunks
playAudio2 :: Simple -> IORef [Double] -> [Float] -> IO()
playAudio2 s bands samples = do
    let buffer = take 4096 samples
    simpleWrite s buffer
    writeIORef bands $ foldFFT 8 $ doFFT $ take 256 samples
    playAudio2 s bands (drop 4096 samples)

sineWaveBands :: IORef Float -> IORef [Double]-> IO ()
sineWaveBands time bands =
    forever $ do
        t <- readIORef time
        writeIORef time $ t + 1
        t <- readIORef time

        writeIORef bands [(sin . realToFrac $ t * 0.002 * pi + i * (2 * pi / 8)  ) | i <- [0..7]] 

-- Entry point for the application
--main = do
--    args <- getArgs
--    case args of
--        [input] -> do
--            bands <- newIORef [1..8]
--            playAudio input bands
--        _ -> putStrLn "Must specify filename"
