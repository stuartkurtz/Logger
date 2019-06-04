module Atomic (
    Atomic,
    newAtomic,
    update,
    updateIO
)
where

import Control.Concurrent.MVar

data Atomic a = Atomic
    { sentinal :: MVar ()
    , value :: MVar a
    }

newAtomic :: a -> IO (Atomic a)
newAtomic a = Atomic <$> newMVar () <*> newMVar a

get :: Atomic a -> IO a
get v = do
    takeMVar (sentinal v)
    takeMVar (value v)

put :: Atomic a -> a -> IO a
put v a = do
    putMVar (value v) a
    putMVar (sentinal v) ()
    pure a

update :: Atomic a -> (a -> a) -> IO a
update v f = do
    a <- get v
    put v (f a)

updateIO :: Atomic a -> (a -> IO a) -> IO a
updateIO v f = do
    a <- get v 
    a' <- f a
    put v a'
