{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A reconstruction of the `operational` package, but using the
-- 'FreeT' monad transformer.
module Control.Monad.Operational
    ( Program
    , ProgramView
    , view
    , interpret
    , interpretWithMonad

    , ProgramT(..)
    , singleton
    , ProgramViewT(..)
    , viewT
    , liftProgram
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Data.Functor.Yoneda.Contravariant

type Program instr = ProgramT instr Identity

type ProgramView instr = ProgramViewT instr Identity

view :: Program instr a -> ProgramView instr a
view = runIdentity . viewT

interpret :: Monad m =>
             (forall x. instr x -> m x)
          -> Program instr a
          -> m a
interpret evalI = eval . view
    where eval (Return a) = return a
          eval (i :>>= k) = evalI i >>= interpret evalI . k

interpretWithMonad :: Monad m =>
                      (forall x. instr x -> m x)
                   -> Program instr a
                   -> m a
interpretWithMonad = interpret

newtype ProgramT instr m a = 
    ProgramT { toFreeT :: FreeT (Yoneda instr) m a }
            deriving (Functor, Applicative, Monad, MonadTrans)

singleton :: Monad m => instr a -> ProgramT instr m a
singleton = ProgramT . liftF . Yoneda id


data ProgramViewT instr m a where
    Return :: a -> ProgramViewT instr m a
    (:>>=) :: instr a -> (a -> ProgramT instr m b) -> ProgramViewT instr m b

viewT :: Monad m => ProgramT instr m a -> m (ProgramViewT instr m a)
viewT = liftM eval . runFreeT . toFreeT
    where eval (Pure a) = Return a
          eval (Free (Yoneda f i)) = i :>>= (ProgramT . f)

liftProgram :: Monad m => Program instr a -> ProgramT instr m a
liftProgram = ProgramT . hoistFreeT (return . runIdentity) . toFreeT
