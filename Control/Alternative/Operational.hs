{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Alternative.Operational 
    ( ProgramA
    , singleton
    , interpretA

    , ProgramViewA(..)
    , viewA
    , compileA
    , transformA
    ) where

import Control.Applicative
import Control.Alternative.Free hiding (Pure)
import Data.Functor.Yoneda.Contravariant

newtype ProgramA instr a =
    ProgramA { unProgramA :: Alt (Yoneda instr) a }
             deriving (Functor, Applicative, Alternative)

interpretA :: forall instr f a. Alternative f =>
              (forall x. instr x -> f x)
           -> ProgramA instr a 
           -> f a
interpretA evalI = runAlt evalF . unProgramA
    where evalF :: forall x. Yoneda instr x -> f x
          evalF (Yoneda k i) = fmap k (evalI i)

-- | Turn an instruction into an action.
singleton :: instr a -> ProgramA instr a
singleton = ProgramA . liftAlt . Yoneda id




data ProgramViewA instr a where
    Pure   :: a -> ProgramViewA instr a
    Instr  :: instr a -> ProgramViewA instr a
    (:<*>) :: ProgramViewA instr (a -> b) 
           -> ProgramViewA instr a
           -> ProgramViewA instr b
    Empty  :: ProgramViewA instr a
    (:<|>) :: ProgramViewA instr a 
           -> ProgramViewA instr a
           -> ProgramViewA instr a

infixl 4 :<*>
infixl 3 :<|>

instance Functor (ProgramViewA instr) where
    fmap f (Pure a) = Pure (f a)
    fmap f (Instr i) = Pure f :<*> Instr i
    fmap f (ff :<*> fa) = ((f .) <$> ff) :<*> fa

instance Applicative (ProgramViewA instr) where
    pure = Pure
    (<*>) = (:<*>)

instance Alternative (ProgramViewA instr) where
    empty = Empty
    (<|>) = (:<|>)


viewA :: ProgramA instr a -> ProgramViewA instr a
viewA = interpretA Instr

-- | The inverse of 'viewA'.
compileA :: ProgramViewA instr a -> ProgramA instr a
compileA (Pure a) = pure a
compileA (Instr i) = singleton i
compileA (ff :<*> fa) = compileA ff <*> compileA fa

transformA :: (ProgramViewA instr a -> ProgramViewA instr' a')
           -> ProgramA instr a -> ProgramA instr' a'
transformA k = compileA . k . viewA
