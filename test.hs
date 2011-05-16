
-- Generates test wave bands
sineWaveBands :: Int -> IORef Float -> IORef [Double]-> IO ()
sineWaveBands num time bands =
    forever $ do
        t <- readIORef time
        writeIORef time $ t + 1
        t <- readIORef time

        writeIORef bands [(sin . realToFrac $ t * 0.002 * pi + i * (2 * pi / num)  ) | i <- [0.. (num-1)]]
        
        
-- Entry point for the application
main = do
    args <- getArgs
    case args of
        [input] -> do
            bands <- newIORef [1..8]
            playAudio input bands
        _ -> putStrLn "Must specify filename"