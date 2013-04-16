{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A simpler, non-transformer version of this package's
-- "Control.Monad.Operational"\'s 'Program' type, using 'Free'
-- directly.
module Control.Monad.Operational.Simple 
    ( module Control.Operational.Class
    , Program(..)
    , interpret
    , fromProgram
    , ProgramView(..)
    , view
    ) where

import Control.Applicative
import Control.Monad.Free
import Control.Operational.Class
import Data.Functor.Yoneda.Contravariant


newtype Program instr a = 
    Program { -- | Intepret the program as a 'Free' monad.
              toFree :: Free (Yoneda instr) a 
            } deriving (Functor, Applicative, Monad)

instance Operational instr (Program instr) where
    singleton = Program . liftF . liftYoneda

-- | Interpret a 'Program' by translating each instruction to a
-- 'Monad' action.  Does not use 'view'.
interpret :: forall m instr a. (Functor m, Monad m) => 
             (forall x. instr x -> m x)
          -> Program instr a
          -> m a
interpret evalI = retract . hoistFree evalF . toFree
    where evalF :: forall x. Yoneda instr x -> m x
          evalF (Yoneda f i) = fmap f (evalI i)

-- | Lift a 'Program' to any 'Operational' instance at least as
-- powerful as 'Monad'.
fromProgram :: (Operational instr (p instr), 
                Functor (p instr), Monad (p instr)) => 
               Program instr a -> p instr a
fromProgram = interpret singleton

data ProgramView instr a where
    Return :: a -> ProgramView instr a
    (:>>=) :: instr a -> (a -> Program instr b) -> ProgramView instr b

view :: Program instr a -> ProgramView instr a
view = eval . toFree 
    where eval (Pure a) = Return a
          eval (Free (Yoneda f i)) = i :>>= (Program . f)
