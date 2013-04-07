{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | @operational@-style 'Alternative' programs.  See
-- "Control.Applicative.Operational" for guidance on how to use this
-- module.
module Control.Alternative.Operational 
    ( ProgramA(..)
    , singleton
    , interpretA

    , ProgramViewA(..)
    , viewA
    ) where

import Control.Applicative
import qualified Control.Alternative.Free as Free
import Control.Alternative.Free hiding (Pure)
import Data.Functor.Yoneda.Contravariant

newtype ProgramA instr a =
    ProgramA { -- | Interpret the program as a free 'Alternative' ('Alt').
               toAlt :: Alt (Yoneda instr) a 
             } deriving (Functor, Applicative, Alternative)

interpretA :: forall instr f a. Alternative f =>
              (forall x. instr x -> f x)
           -> ProgramA instr a 
           -> f a
interpretA evalI = runAlt evalF . toAlt
    where evalF :: forall x. Yoneda instr x -> f x
          evalF (Yoneda k i) = fmap k (evalI i)

singleton :: instr a -> ProgramA instr a
singleton = ProgramA . liftAlt . Yoneda id


data ProgramViewA instr a where
    Pure   :: a -> ProgramViewA instr a
    (:<**>) :: instr a
            -> ProgramViewA instr (a -> b) 
            -> ProgramViewA instr b
    Empty  :: ProgramViewA instr a
    (:<|>) :: ProgramViewA instr a 
           -> ProgramViewA instr a
           -> ProgramViewA instr a

-- this is the same fixity as '<**>' and '<|>'; dunno why it's not infixr
infixl 4 :<**>
infixl 3 :<|>

viewA :: ProgramA instr a -> ProgramViewA instr a
viewA = viewA' . toAlt

viewA' :: Alt (Yoneda instr) a -> ProgramViewA instr a
viewA' (Free.Pure a) = Pure a
viewA' (Free.Ap (Yoneda f i) next) = i :<**> viewA' (fmap (.f) next)
viewA' (Free.Alt xs) = foldr (:<|>) Empty (map viewA' xs)
