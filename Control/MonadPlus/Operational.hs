{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | @operational@-style programs for 'MonadPlus'.  See the
-- documentation for "Control.Applicative.Operational" and
-- "Control.Monad.Operational" for guidance on how to use this module.
module Control.MonadPlus.Operational
    ( module Control.Operational.Class
    , ProgramP(..)
    , interpretP
    , fromProgramP
      
    , ProgramViewP(..)
    , view
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Free
import Control.Operational.Class
import Data.Functor.Coyoneda

newtype ProgramP instr a = 
    ProgramP { -- | Interpret the program as a free 'MonadPlus'.
               toFree :: FreeT (Coyoneda instr) [] a
             } deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Operational instr (ProgramP instr) where
    singleton = ProgramP . liftF . liftCoyoneda


retractPlus :: MonadPlus f => FreeT f [] a -> f a
retractPlus (FreeT m) = foldr (mplus . go) mzero m where
    go (Pure a)  = return a
    go (Free as) = as >>= retractPlus

interpretP :: forall m instr a. (Functor m, MonadPlus m) => 
              (forall x. instr x -> m x)
           -> ProgramP instr a
           -> m a
interpretP evalI = retractPlus . transFreeT evalF . toFree
    where evalF :: forall x. Coyoneda instr x -> m x
          evalF (Coyoneda f i) = fmap f (evalI i)

fromProgramP
    :: (Operational instr m, Functor m, MonadPlus m) => ProgramP instr a -> m a
fromProgramP = interpretP singleton


data ProgramViewP instr a where
    Return :: a -> ProgramViewP instr a
    (:>>=) :: instr a -> (a -> ProgramP instr b) -> ProgramViewP instr b
    MPlus  :: [ProgramViewP instr a] -> ProgramViewP instr a

view :: ProgramP instr a -> ProgramViewP instr a
view = eval . toFree 
    where eval' (Pure a) = Return a
          eval' (Free (Coyoneda f i)) = i :>>= (ProgramP . f)
          eval (FreeT mas) = MPlus $ map eval' mas
