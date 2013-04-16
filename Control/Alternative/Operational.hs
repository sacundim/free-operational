{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | @operational@-style 'Alternative' programs.  See
-- "Control.Applicative.Operational" for guidance on how to use this
-- module.
module Control.Alternative.Operational 
    ( module Control.Operational.Class
    , ProgramAlt(..)
    , interpretAlt
    , fromProgramAlt

    , ProgramViewAlt(..)
    , viewAlt
    ) where

import Control.Applicative
import qualified Control.Alternative.Free as Free
import Control.Alternative.Free hiding (Pure)
import Control.Operational.Class
import Data.Functor.Yoneda.Contravariant

newtype ProgramAlt instr a =
    ProgramAlt { -- | Interpret the program as a free 'Alternative' ('Alt').
                 toAlt :: Alt (Yoneda instr) a 
               } deriving (Functor, Applicative, Alternative)

instance Operational instr (ProgramAlt instr) where
    singleton = ProgramAlt . liftAlt . liftYoneda


interpretAlt :: forall instr f a.
                Alternative f =>
               (forall x. instr x -> f x)
             -> ProgramAlt instr a 
             -> f a
interpretAlt evalI = runAlt evalF . toAlt
    where evalF :: forall x. Yoneda instr x -> f x
          evalF (Yoneda k i) = fmap k (evalI i)

fromProgramAlt :: (Operational instr (p instr), Alternative (p instr)) => 
                  ProgramAlt instr a -> p instr a
fromProgramAlt = interpretAlt singleton

data ProgramViewAlt instr a where
    Pure    :: a -> ProgramViewAlt instr a
    (:<**>) :: instr a
            -> ProgramViewAlt instr (a -> b) 
            -> ProgramViewAlt instr b
    Empty   :: ProgramViewAlt instr a
    (:<|>)  :: ProgramViewAlt instr a 
            -> ProgramViewAlt instr a
            -> ProgramViewAlt instr a

-- this is the same fixity as '<**>' and '<|>'; dunno why it's not infixr
infixl 4 :<**>
infixl 3 :<|>

viewAlt :: ProgramAlt instr a -> ProgramViewAlt instr a
viewAlt = viewAlt' . toAlt

viewAlt' :: Alt (Yoneda instr) a -> ProgramViewAlt instr a
viewAlt' (Free.Pure a) = Pure a
viewAlt' (Free.Ap (Yoneda f i) next) = i :<**> viewAlt' (fmap (.f) next)
viewAlt' (Free.Alt xs) = foldr (:<|>) Empty (map viewAlt' xs)
