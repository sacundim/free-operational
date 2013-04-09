{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | @operational@-style programs for 'MonadPlus'.  See the
-- documentation for "Control.Applicative.Operational" and
-- "Control.Monad.Operational" for guidance on how to use this module.
module Control.MonadPlus.Operational
    ( Program(..)
    , singleton
    , interpret
      
    , ProgramView(..)
    , view
    ) where

import Control.Applicative
import Control.Monad
import Control.MonadPlus.Free
import Data.Functor.Yoneda.Contravariant

newtype Program instr a = 
    Program { -- | Interpret the program as a free 'MonadPlus'.
              toFree :: Free (Yoneda instr) a 
            } deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

singleton :: instr a -> Program instr a
singleton = Program . liftF . Yoneda id

interpret :: forall m instr a. (Functor m, MonadPlus m) => 
             (forall x. instr x -> m x)
          -> Program instr a
          -> m a
interpret evalI = retract . hoistFree evalF . toFree
    where evalF :: forall x. Yoneda instr x -> m x
          evalF (Yoneda f i) = fmap f (evalI i)


data ProgramView instr a where
    Return :: a -> ProgramView instr a
    (:>>=) :: instr a -> (a -> Program instr b) -> ProgramView instr b
    MEmpty :: ProgramView instr a
    MPlus :: ProgramView instr a -> ProgramView instr a -> ProgramView instr a

view :: Program instr a -> ProgramView instr a
view = eval . toFree 
    where eval (Pure a) = Return a
          eval (Free (Yoneda f i)) = i :>>= (Program . f)
          eval (Plus mas) = foldr MPlus MEmpty (map eval mas)
