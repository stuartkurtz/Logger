module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.List (intercalate)
import Data.Traversable (for)
import GHC.Conc
import System.Random (randomRIO)

import Logger

myTask :: String -> Logger -> IO ()
myTask a logger = void $ do
    for [1..100] $ \ix -> do
        delay <- randomRIO (1,100)
        threadDelay delay
        message logger $ intercalate " " [a,show ix]

main :: IO ()
main = do
    nproc <- getNumProcessors
    let ncap = (nproc - 2) `max` 1
    setNumCapabilities ncap
    putStrLn $ "Capacities: " ++ show ncap

    logger <- newLogger
    let startTask msg = logTask logger (myTask msg)
        msgs = map (:[]) ['a'..'z']

    for msgs startTask
    wait logger
