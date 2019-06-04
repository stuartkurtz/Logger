module Locked where

import Control.Concurrent.MVar

data Locked a = Locked
    { sentinal :: MVar ()
    , value :: MVar a
    }

newLock :: a -> IO (Locked a)
newLock a = Locked <$> newMVar () <*> newMVar a

get :: Locked a -> IO a
get v = do
    takeMVar (sentinal v)
    takeMVar (value v)

put :: Locked a -> a -> IO a
put v a = do
    putMVar (value v) a
    putMVar (sentinal v) ()
    pure a

update :: Locked a -> (a -> a) -> IO a
update v f = do
    a <- get v
    put v (f a)

updateIO :: Locked a -> (a -> IO a) -> IO a
updateIO v f = do
    a <- get v 
    a' <- f a
    put v a'
