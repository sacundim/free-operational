{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Operational
    ( ProgramT(..)
    , interpretT
    , ProgramViewT(..)
    , viewT
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Operational.Class
import Data.Functor.Yoneda.Contravariant


newtype ProgramT instr m a = 
    ProgramT { toFreeT :: FreeT (Yoneda instr) m a 
             } deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => Operational instr (ProgramT instr m) where
    singleton = ProgramT . liftF . liftYoneda

-- | Given an intepretation of @instr x@ as actions over a given monad
-- transformer @t@ (transforming over an arbitrary monad @m@),
-- interpret @'ProgramT' instr@ as a monad transformer @t@.  Read that
-- sentence and the type carefully: the instruction interpretation
-- picks @t@ but doesn't pick @m@.
interpretT
    :: forall t m instr a. 
       (MonadTrans t, Functor (t m), Monad (t m), Functor m, Monad m) => 
       (forall m x. (Functor m, Monad m) => instr x -> t m x)
    -> ProgramT instr m a -> t m a
interpretT evalI = retractT . transFreeT evalF . toFreeT
    where evalF :: forall m x.
                   (Functor (t m), Monad (t m), Functor m, Monad m) => 
                   Yoneda instr x -> t m x
          evalF (Yoneda f i) = fmap f (evalI i)

retractT :: (MonadTrans t, Functor (t m), Monad (t m), Monad m) => 
            FreeT (t m) m a -> t m a
retractT = retractT' . hoistFreeT lift
    where retractT' :: Monad m => FreeT m m a -> m a
          retractT' prog = do fab <- runFreeT prog
                              case fab of
                                Pure a  -> return a
                                Free fb -> join $ liftM retractT' fb

{- 
import Control.Monad.Trans.Reader as Canon

data ReaderI r a where
    Ask :: ReaderI r r

runReaderT :: forall r m a. 
              (Functor m, Monad m) => 
              ProgramT (ReaderI r) m a -> Canon.ReaderT r m a
runReaderT = interpretT evalI
    where evalI :: forall m x.
                   (Functor m, Monad m) =>
                   ReaderI r x -> Canon.ReaderT r m x
          evalI Ask = Canon.ask
-}


data ProgramViewT instr m a where
    Return :: a -> ProgramViewT instr m a
    (:>>=) :: instr a -> (a -> ProgramT instr m b) -> ProgramViewT instr m b

infixl 1 :>>=

viewT :: Monad m => ProgramT instr m a -> m (ProgramViewT instr m a)
viewT = liftM eval . runFreeT . toFreeT
    where eval (Pure a) = Return a
          eval (Free (Yoneda f i)) = i :>>= ProgramT . f

