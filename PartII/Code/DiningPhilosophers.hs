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

-- Prints to screen "Time | Philosopher Intent"
printPhilosopherIntent :: String -> String -> IO ()
printPhilosopherIntent name i = do
                                    timeString <- getTimeString
                                    putStrLn (timeString ++ " | " ++ name ++ " is " ++ i)

-- Prints to screen "Time | Delaying Philosopher s Seconds" and delays the thread
delayPhilosopherPrint :: String -> Int -> IO ()
delayPhilosopherPrint name s = do
                                    timeString <- getTimeString
                                    putStrLn (timeString ++ " | Delaying " ++ name ++ " " ++ show(s) ++ " seconds")
                                    threadDelay (μstos s)

-- A list of names of philosophers
philosophers :: [String]
philosophers = ["Socrates", "Kant", "Aristotle", "Descartes", "Plato", "Aquinas", "Marx"]

-- Doesn't need to be TMVar Int

philosopher :: String -> ((TMVar Int), (TMVar Int)) -> IO ()
philosopher name (ls,rs) = do
    -- Philosopher is thinking
    -- Wait for a random number of seconds and announce you are doing so
    printPhilosopherIntent name "thinking"
    seconds <- randomRIO (1, 10)
    delayPhilosopherPrint name seconds

    -- Philosopher is hungry
    -- Attempt to get both sporks together (will wait until it can get both)
    printPhilosopherIntent name "hungry"
    (l, r) <- atomically $ do
        l <- takeTMVar ls
        r <- takeTMVar rs
        return (l, r)

    -- Philosopher is eating
    -- Philosopher has both sporks so they can eat for a random number of seconds
    printPhilosopherAction name " is eating"
    seconds <- randomRIO (1, 10)
    printPhilosopherDelay name seconds

    -- Philosopher has finished eating so put both forks back together.
    atomically $ do
        putTMVar ls l
        putTMVar rs r

    -- Loop the philosopher so they are thinking again
    philosopher name (ls,rs)

main = do
    -- Create a list of sporks for the philosophers to use
    -- Used sporks as to not confuse with threads forking
    sporks <- mapM newTMVarIO [1..length(philosophers)]
    -- Create a pair of sporks left and right of a philosopher
    let pairs = zip sporks (tail(sporks) ++ [(head sporks)])

    -- Fork all of the philosophers with the a name and a pair
    forkIO (philosopher (philosophers!!0) (pairs!!0))
    forkIO (philosopher (philosophers!!1) (pairs!!1))
    forkIO (philosopher (philosophers!!2) (pairs!!2))
    forkIO (philosopher (philosophers!!3) (pairs!!3))
    forkIO (philosopher (philosophers!!4) (pairs!!4))
    forkIO (philosopher (philosophers!!5) (pairs!!5))
    forkIO (philosopher (philosophers!!6) (pairs!!6))

    -- Used to delay main forever so that the philosophers can run
    -- Need to use Ctrl+C to close the program
    forever $ threadDelay (μstos 1)
