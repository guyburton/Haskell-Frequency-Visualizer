
import Numeric.FFT
import Data.WAVE
import System.Environment (getArgs)

-- Gets a list of samples for the specified channel from the wave file
getChannel :: Int -> WAVE -> [Double]
getChannel channel wave = map (\x -> sampleToDouble $ x !! channel) $ waveSamples wave

-- Plays a specified sound file
playSound :: String -> IO()
playSound file = do
    putStrLn $ "playing sound " ++ file
    wave <- getWAVEFile file
    print $ take 256 $ getChannel 1 wave

-- Entry point for the application
main = do
    args <- getArgs
    case args of
        [input] -> playSound input
        _ -> putStrLn "One argument required (wav file name)"
