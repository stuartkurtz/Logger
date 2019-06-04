module Logger where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set

import Locked

data LogCommand
    = Attach ThreadId
    | Message String
    | Detach ThreadId

data Logger = Logger
    { command :: MVar LogCommand
    , clients :: Locked (Set ThreadId)
    , done    :: MVar ()
    }

newLogger :: IO Logger
newLogger = do
    logger <-
        Logger <$> newEmptyMVar <*> newLock Set.empty <*> newEmptyMVar
    forkIO . fix $ \loop -> do
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
    pure logger

attach :: Logger -> ThreadId -> IO ()
attach logger thread = putMVar (command logger) (Attach thread)

detach :: Logger -> ThreadId -> IO ()
detach logger thread = putMVar (command logger) (Detach thread)

message :: Logger -> String -> IO ()
message logger msg = putMVar (command logger) (Message msg)

wait :: Logger -> IO ()
wait logger = takeMVar (done logger)
