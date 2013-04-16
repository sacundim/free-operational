{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Operational
    ( ProgramT(..)
    , interpretT
    , interpretTM
    , interpretM
    , ProgramViewT(..)
    , viewT
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Operational.Class
import Control.Operational.Instruction
import Data.Functor.Yoneda.Contravariant


newtype ProgramT instr m a = 
    ProgramT { toFreeT :: FreeT (Yoneda instr) m a 
             } deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => Operational instr (ProgramT instr m) where
    singleton = ProgramT . liftF . liftInstr

-- | Given an intepretation of @instr x@ as actions over a given monad
-- transformer @t@ (transforming over an arbitrary monad @m@),
-- interpret @'ProgramT' instr@ as a monad transformer @t@.  Read that
-- sentence and the type carefully: the instruction interpretation can
-- pick its choice of @t@ but not @m@.
interpretT
    :: forall t m instr a. 
       (MonadTrans t, Functor (t m), Monad (t m), Functor m, Monad m) => 
       (forall n x. (Functor n, Monad n) => instr x -> t n x)
    -> ProgramT instr m a -> t m a
interpretT evalI = retractT . transFreeT evalF . toFreeT
    where evalF :: forall m x.
                   (Functor (t m), Monad (t m), Functor m, Monad m) => 
                   Yoneda instr x -> t m x
          evalF (Yoneda f i) = fmap f (evalI i)

retractT :: (MonadTrans t, Functor (t m), Monad (t m), Monad m) => 
            FreeT (t m) m a -> t m a
retractT = retract . hoistFreeT lift

retract :: Monad m => FreeT m m a -> m a
retract prog = do fab <- runFreeT prog
                  case fab of
                    Pure a  -> return a
                    Free fb -> join $ liftM retract fb

-- | Given an intepretation of @instr x@ as actions over a given
-- transformed monad @t m@, interpret @'ProgramT' instr@ as a
-- transformed monad @t m@.  Read that sentence and the type
-- carefully: the instruction interpretation can pick its choice of
-- both @t@ and @m@.
interpretTM :: (MonadTrans t, Functor (t m), Monad (t m), Monad m) => 
               (forall x. instr x -> t m x) -> ProgramT instr m a -> t m a
interpretTM evalI = retractT . transFreeT (liftEvalI evalI) . toFreeT

interpretM :: (Functor m, Monad m) => 
              (forall x. instr x -> m x) -> ProgramT instr m a -> m a
interpretM evalI = retract . transFreeT (liftEvalI evalI) . toFreeT


data ProgramViewT instr m a where
    Return :: a -> ProgramViewT instr m a
    (:>>=) :: instr a -> (a -> ProgramT instr m b) -> ProgramViewT instr m b

infixl 1 :>>=

viewT :: Monad m => ProgramT instr m a -> m (ProgramViewT instr m a)
viewT = liftM eval . runFreeT . toFreeT
    where eval (Pure a) = Return a
          eval (Free (Yoneda f i)) = i :>>= ProgramT . f

