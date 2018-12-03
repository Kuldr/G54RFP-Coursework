module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)

-- Convert seconds to microseconds
μstos :: Int -> Int
μstos i = i * 1000000

-- Gets a formatted string of the current time
getTimeString :: IO String
getTimeString = do
                    zt <- getZonedTime
                    return $ formatTime defaultTimeLocale "%H:%M:%S" zt

-- Prints to screen "Time | Philosipher Action"
printPhilosipherAction :: String -> String -> IO ()
printPhilosipherAction name a = do
                                    timeString <- getTimeString
                                    putStrLn (timeString ++ " | " ++ name ++ a)

-- Prints to screen "Time | Delaying Philosipher s Seconds" and delays the thread
printPhilosipherDelay :: String -> Int -> IO ()
printPhilosipherDelay name s = do
                                    timeString <- getTimeString
                                    putStrLn (timeString ++ " | Delaying " ++ name ++ " " ++ show(s) ++ " seconds")
                                    threadDelay (μstos s)
                                    
-- Doesn't need to be TMVar Int

philosiphers :: [String]
philosiphers = ["Socrates", "Kant", "Aristotle", "Descartes", "Plato", "Aquinas", "Marx"]

philosipher :: String -> ((TMVar Int), (TMVar Int)) -> IO ()
philosipher name (ls,rs) = do
    -- Philosipher is thinking
    -- Wait for a random number of seconds and announce you are doing so
    printPhilosipherAction name " is thinking"
    seconds <- randomRIO (1, 10)
    printPhilosipherDelay name seconds

    -- Philosipher is hungry
    -- Attempt to get both sporks together (will wait until it can get both)
    printPhilosipherAction name " is hungry"
    (l, r) <- atomically $ do
        l <- takeTMVar ls
        r <- takeTMVar rs
        return (l, r)

    -- Philosipher is eating
    -- Philosipher has both sporks so they can eat for a random number of seconds
    printPhilosipherAction name " is eating"
    seconds <- randomRIO (1, 10)
    printPhilosipherDelay name seconds

    -- Philosipher has finished eating so put both forks back together.
    atomically $ do
        putTMVar ls l
        putTMVar rs r

    -- Loop the philosipher so they are thinking again
    philosipher name (ls,rs)

main = do
    -- Create a list of sporks for the philosiphers to use
    -- Used sporks as to not confuse with threads forking
    sporks <- mapM newTMVarIO [1..length(philosiphers)]
    -- Create a pair of sporks left and right of a philosipher
    let pairs = zip sporks (tail(sporks) ++ [(head sporks)])

    -- Fork all of the philosiphers with the a name and a pair
    forkIO (philosipher (philosiphers!!0) (pairs!!0))
    forkIO (philosipher (philosiphers!!1) (pairs!!1))
    forkIO (philosipher (philosiphers!!2) (pairs!!2))
    forkIO (philosipher (philosiphers!!3) (pairs!!3))
    forkIO (philosipher (philosiphers!!4) (pairs!!4))
    forkIO (philosipher (philosiphers!!5) (pairs!!5))
    forkIO (philosipher (philosiphers!!6) (pairs!!6))

    -- Used to delay main forever so that the philosiphers can run
    -- Need to use Ctrl+C to close the program
    forever $ threadDelay (μstos 1)
