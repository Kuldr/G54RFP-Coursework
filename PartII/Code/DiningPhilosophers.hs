module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random

-- Doesn't need to be TMVar Int

philosiphers :: [String]
philosiphers = ["Socrates", "Kant", "Aristotle", "Descartes", "Plato", "Aquinas", "Marx"]

philosipher :: String -> ((TMVar Int), (TMVar Int)) -> IO ()
philosipher name (ls,rs) = do
    -- Philosipher is thinking
    putStrLn (name ++ " is thinking")
    seconds <- randomRIO (1, 10)
    putStrLn ("Delaying " ++ name ++ " " ++ show(seconds) ++ " seconds")
    threadDelay (seconds*1000000)

    -- Philosipher is hungry, try to get both sporks
    putStrLn (name ++ " is hungry")
    (l, r) <- atomically $ do
        l <- takeTMVar ls
        r <- takeTMVar rs
        return (l, r)

    -- Philosipher has sporks so they can eat
    putStrLn (name ++ " is eating")
    seconds <- randomRIO (1, 10)
    putStrLn ("Delaying " ++ name ++ " " ++ show(seconds) ++ " seconds")
    threadDelay (seconds*1000000)

    -- Philosipher puts the sporks back
    atomically $ do
        putTMVar ls l
        putTMVar rs r

    -- Loop the philosipher so they are thinking again
    philosipher name (ls,rs)

main = do
    sporks <- mapM newTMVarIO [1..length(philosiphers)]
    let pairs = zip sporks (tail(sporks) ++ [(head sporks)])

    forkIO (philosipher (philosiphers!!0) (pairs!!0))
    forkIO (philosipher (philosiphers!!1) (pairs!!1))
    forkIO (philosipher (philosiphers!!2) (pairs!!2))
    forkIO (philosipher (philosiphers!!3) (pairs!!3))
    forkIO (philosipher (philosiphers!!4) (pairs!!4))
    forkIO (philosipher (philosiphers!!5) (pairs!!5))
    forkIO (philosipher (philosiphers!!6) (pairs!!6))

    done <- newEmptyMVar
    takeMVar done
