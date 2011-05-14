
import Data.Complex
import Numeric.FFT
import Data.WAVE
import System.Environment (getArgs)
import Control.Monad

-- Gets a list of samples for the specified channel from the wave file
getChannel :: Int -> WAVE -> [Double]
getChannel channel wave = map (\x -> sampleToDouble $ x !! channel) $ waveSamples wave

-- Plays a specified sound file
plotSound :: WAVE -> String
plotSound wave = concat $ map (\x -> show x ++ "\n") $ getChannel 1 wave

plotFFT :: WAVE -> Int -> String
plotFFT wave windowSize = ((++) startcommands . concat $ map (\x -> show (magnitude x) ++ "\n") $ doFFT wave windowSize) ++ endcommands
    where
    startcommands = "set title \"FFT of waveform\"\n" ++
                    "set xlabel \"Frequency\"\n" ++
                    "set ylabel \"Magnitude\"\n" ++ 
                    "plot \"-\"\n"
    endcommands =   "end\n" ++
                    "pause -1\n"
doFFT :: WAVE -> Int -> [Complex Double]
doFFT wave windowSize = take ((windowSize `div` 2) - 2) $ fft $ map (\x -> x :+ 0) $ take windowSize $ getChannel 1 wave

-- Entry point for the application
main = do
    args <- getArgs
    case args of
        
        [input] -> do
            wave <- getWAVEFile input
            putStrLn $ plotFFT wave 256
