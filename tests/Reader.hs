{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts #-}

module Reader 
    ( ReaderI(..)
    , ask
    , evalReaderI
    ) where

import Control.Operational.Class
import Test.QuickCheck

data ReaderI r a where
    Ask :: ReaderI r r

instance Show (ReaderI r a) where
    show Ask = "Ask"

instance Arbitrary (ReaderI r r) where
    arbitrary = return Ask

ask :: Operational (ReaderI r) f => f r
ask = singleton Ask

evalReaderI :: ReaderI r a -> r -> a
evalReaderI Ask = id
