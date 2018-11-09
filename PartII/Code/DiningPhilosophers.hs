import Control.Concurrent
import System.Random

-- https://www.rosettacode.org/wiki/Pick_random_element#Haskell
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

philosiphers :: [String]
philosiphers = ["Socrates", "Kant", "Aristotle", "Descartes", "Plato", "Aquinas", "Marx"]

actions :: [String]
actions = [" is hungry", " is thinking", " is eating"]

philosipher :: String -> IO String
philosipher s = do
                act <- pick actions
                return $ s ++ act



-- countFromTo :: Int -> Int -> IO ()
-- countFromTo m n | m > n = return ()
--                 | otherwise = do
--                     putStrLn (show m )
--                     countFromTo (m + 1) n
--
-- hi :: IO ()
-- hi = do
--     start <- newEmptyMVar
--     done  <- newEmptyMVar
--     forkIO $ do
--         takeMVar start
--         countFromTo 1 10
--         putMVar done ()
--     putStrLn "Go!"
--     putMVar start ()
--     takeMVar done
--     countFromTo 11 20
--     putStrLn "Done!"
