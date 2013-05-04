module Main
    ( main
    ) where

import qualified Properties.Applicative as Ap

import Test.Framework (Test, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "applicative" Ap.tests ]
