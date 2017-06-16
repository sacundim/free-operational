{-# LANGUAGE Rank2Types #-}

-- | Utility functions for working with instructions and instruction sets.
--
-- The "Data.Functor.Sum" module is very useful with instruction sets, so this
-- module reexports it.  The 'Sum type can be used to take the union of two
-- instruction sets, and the 'coproduct' function can be used to construct an
-- instruction evaluation for such an union.  So if we have these two
-- instruction evaluations:
--
-- > evalI  :: forall x. instr  x -> f x
-- > evalI' :: forall x. instr' x -> f x
--
-- then their 'coproduct' is an evaluation for the union of the
-- instruction sets:
--
-- > coproduct evalI evalI' :: forall x. (Sum instr instr' x) -> f x
module Control.Operational.Instruction
    ( module Data.Functor.Sum
    , coproduct
    , liftEvalI
    , liftInstr
    ) where

import Data.Functor.Coyoneda
import Data.Functor.Sum

-- | Eliminate a 'Sum'
--
-- From the source of Control.Comonad
coproduct :: (f a -> b) -> (g a -> b) -> Sum f g a -> b
coproduct f _ (InL x) = f x
coproduct _ g (InR y) = g y
{-# INLINE coproduct #-}

-- | Lift an operational instruction evaluator into a free 'Functor'
-- evaluator.
liftEvalI :: Functor f =>
             (forall x. instr x -> f x)
          -> Coyoneda instr a -> f a
liftEvalI evalI (Coyoneda f i) = fmap f (evalI i)

liftInstr :: instr a -> Coyoneda instr a
liftInstr = liftCoyoneda
