{-# LANGUAGE MultiParamTypeClasses #-}

module ThreadSafeList
    ( ThreadSafeList
    , newEmptyList
    , toPureList
    , add
    , remove
    , contains
    ) where

class ThreadSafeList l a where
    newEmptyList :: IO (l a)
    toPureList :: l a -> IO [a]
    add :: l a -> a -> IO Bool
    remove :: l a -> a -> IO Bool
    contains :: l a -> a -> IO Bool
