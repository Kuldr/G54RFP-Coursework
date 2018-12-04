--Dining Philosophers-----------------------------------------------------------
-- Helper Functions
ustos :: Int -> Int
ustos i = i * 1000000

getTimeString :: IO String
getTimeString = do
                    zt <- getZonedTime
                    return $ formatTime defaultTimeLocale "%H:%M:%S" zt

printPhilosopherIntent :: String -> String -> IO ()
printPhilosopherIntent name i =
    do
        timeString <- getTimeString
        putStrLn (timeString ++ " | " ++ name ++ " is " ++ i)

delayPhilosopherPrint :: String -> Int -> IO ()
delayPhilosopherPrint name s =
    do
        timeString <- getTimeString
        putStrLn (timeString ++ " | Delaying " ++ name ++
                    " " ++ show(s) ++ " seconds")
        threadDelay (ustos s)

philosopher :: String -> ((TMVar Int), (TMVar Int)) -> IO ()
philosopher name (ls,rs) =
    do
    -- Work happens here
    philosopher name (ls,rs)

printPhilosopherIntent name "thinking"
seconds <- randomRIO (1, 10)
delayPhilosopherPrint name seconds

printPhilosopherIntent name "hungry"
(l, r) <- atomically $ do
    l <- takeTMVar ls
    r <- takeTMVar rs
    return (l, r)

printPhilosopherIntent name "eating"
seconds <- randomRIO (1, 10)
delayPhilosopherPrint name seconds
atomically $ do
    putTMVar ls l
    putTMVar rs r

main = do
    sporks <- mapM newTMVarIO [1..length(philosophers)]
    let pairs = zip sporks (tail(sporks) ++ [(head sporks)])

    mapM forkIO $ zipWith philosopher philosophers pairs

    forever $ threadDelay (ustos 1)
