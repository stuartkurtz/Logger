module Main where

import Control.Concurrent
import Control.Monad
import Data.List (intercalate)
import System.Random (randomRIO)

import Logger

myTask :: String -> Logger -> IO ()
myTask a logger = do
    forM_ [1..100] $ \ix -> do
        delay <- randomRIO (1,100)
        threadDelay delay
        message logger $ intercalate " " [a,show ix]

logTask :: Logger -> (Logger -> IO ()) -> IO ThreadId
logTask logger task = forkIO $ do
    thread <- myThreadId
    attach logger thread
    task logger
    detach logger thread
    
main :: IO ()
main = do
    logger <- newLogger
    let startTask msg = logTask logger (myTask msg) 
    
    startTask "a"
    startTask "b"
    startTask "c"

    wait logger
