module Logger (
    Logger,
    newLogger,
    message,
    logTask,
    wait
) where

import Control.Concurrent
import Control.Monad (void)
import Data.Set (Set)
import qualified Data.Set as Set

import Atomic

data LogCommand
    = Attach ThreadId
    | Message String
    | Detach ThreadId

data Logger = Logger
    { command :: MVar LogCommand
    , clients :: Atomic (Set ThreadId)
    , done    :: MVar ()
    }

newLogger :: IO Logger
newLogger = do
    logger <- Logger
               <$> newEmptyMVar
               <*> newAtomic Set.empty
               <*> newEmptyMVar

    let loop = do
            cmd <- takeMVar (command logger)
            case cmd of
                Attach thread -> void $ do
                    update (clients logger) (Set.insert thread)
                    loop
                Detach thread -> do
                    clts <- update (clients logger) (Set.delete thread)
                    if null clts
                    then do
                        putStrLn "logger: done"
                        putMVar (done logger) ()
                    else do
                        loop
                Message msg -> do
                    putStrLn msg
                    loop

    forkIO loop
    pure logger

attach :: Logger -> ThreadId -> IO ()
attach logger thread = putMVar (command logger) (Attach thread)

detach :: Logger -> ThreadId -> IO ()
detach logger thread = putMVar (command logger) (Detach thread)

message :: Logger -> String -> IO ()
message logger msg = putMVar (command logger) (Message msg)

logTask :: Logger -> (Logger -> IO ()) -> IO ThreadId
logTask logger task = forkIO $ do
    thread <- myThreadId
    attach logger thread
    task logger
    detach logger thread

wait :: Logger -> IO ()
wait logger = readMVar (done logger)
