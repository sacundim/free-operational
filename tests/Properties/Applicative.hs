{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances #-}

module Properties.Applicative 
    ( tests
    ) where

import Control.Applicative
import Control.Applicative.Operational
import Control.Monad

import Text.Show.Functions
import Util
import Reader

import Test.QuickCheck
import Test.QuickCheck.Classes ( functor, applicative, monad )
import Test.QuickCheck.Checkers (EqProp(..))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [Test]
tests = [ testProperty "applicative/FunctorLaws" prop_FunctorLaws
        , testProperty "applicative/ApplicativeLaws" prop_ApplicativeLaws
        , testProperty "applicative/InterpretCompile" prop_InterpretCompile
        ]

prop_FunctorLaws :: ProgramAp (ReaderI String) (Int, Char, String)
                 -> Property
prop_FunctorLaws = runBatch . functor

prop_ApplicativeLaws :: ProgramAp (ReaderI String) (Int, Char, String)
                     -> Property
prop_ApplicativeLaws = runBatch . applicative

prop_InterpretCompile :: ProgramAp (ReaderI String) Int -> Property
prop_InterpretCompile prog = prog =-= compileAp (viewAp prog)


runReader :: ProgramAp (ReaderI r) a -> r -> a
runReader = interpretAp evalReaderI

instance (Show r, Arbitrary r, EqProp a) =>
    EqProp (ProgramAp (ReaderI r) a) where
        ra =-= rb = runReader ra =-= runReader rb

instance (CoArbitrary r, Arbitrary a) =>
    Arbitrary (ProgramAp (ReaderI r) a) where
        arbitrary = oneof [ liftM pure arbitrary 
                          , liftM (<*>ask) arbitrary ]

instance Show (ProgramViewAp instr a) where
    show (Pure _) = "Pure _"
    show (_ :<**> k) = "_ :<**> " ++ show k

instance Show (ProgramAp instr a) where
    show = show . viewAp
