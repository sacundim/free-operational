{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A simpler, non-transformer version of this package's
-- "Control.Monad.Operational"\'s 'Program' type, using 'Free'
-- directly.
module Control.Monad.Operational.Simple 
    ( Program(..)
    , singleton
    , interpret
    , ProgramView(..)
    , view
    ) where

import Control.Applicative
import Control.Monad.Free
import Data.Functor.Yoneda.Contravariant


newtype Program instr a = 
    Program { -- | Intepret the program as a 'Free' monad.
              toFree :: Free (Yoneda instr) a 
            } deriving (Functor, Applicative, Monad)

singleton :: instr a -> Program instr a
singleton = Program . liftF . Yoneda id

-- | Interpret a 'Program' by translating each instruction to a
-- 'Monad' action.  Does not use 'view'.
interpret :: forall m instr a. (Functor m, Monad m) => 
             (forall x. instr x -> m x)
          -> Program instr a
          -> m a
interpret evalI = retract . hoistFree evalF . toFree
    where evalF :: forall x. Yoneda instr x -> m x
          evalF (Yoneda f i) = fmap f (evalI i)

data ProgramView instr a where
    Return :: a -> ProgramView instr a
    (:>>=) :: instr a -> (a -> Program instr b) -> ProgramView instr b

view :: Program instr a -> ProgramView instr a
view = eval . toFree 
    where eval (Pure a) = Return a
          eval (Free (Yoneda f i)) = i :>>= (Program . f)
