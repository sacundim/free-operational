module Util 
    ( runBatch
    , runTest
    ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers (TestBatch, unbatch, EqProp(..) )
import qualified Test.QuickCheck.Checkers as Checkers


------------------------------------------------------------------
------------------------------------------------------------------
--
-- Helpers for Test.QuickCheck.Checkers
-- 

runBatch :: TestBatch -> Property
runBatch (name, tests) = 
    whenFail report $ conjoin (map (runTest name) tests) 
        where report = putStrLn $ "Batch '" ++ name ++ "' failed."

runTest :: String -> Checkers.Test -> Property
runTest group (name, property) = 
    whenFail report property
        where report = 
                  putStrLn $ "Test '" ++ group ++ "/" ++ name ++ "' failed."

