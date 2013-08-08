{-# LANGUAGE Rank2Types #-}

-- | Utility functions for working with instructions and instruction sets.
--
-- The "Data.Functor.Coproduct" module is very useful with instruction
-- sets, so this module reexports it.  The 'Coproduct' type can be
-- used to take the union of two instruction sets, and the 'coproduct'
-- function can be used to construct an instruction evaluation for
-- such an union.  So if we have these two instruction evaluations:
--
-- > evalI  :: forall x. instr  x -> f x
-- > evalI' :: forall x. instr' x -> f x 
--
-- then their 'coproduct' is an evaluation for the union of the
-- instruction sets:
-- 
-- > coproduct evalI evalI' :: forall x. (Coproduct instr instr' x) -> f x
module Control.Operational.Instruction 
    ( module Data.Functor.Coproduct
    , liftEvalI
    , liftInstr
    ) where

import Data.Functor.Coproduct
import Data.Functor.Yoneda.Reduction

-- | Lift an operational instruction evaluator into a free 'Functor'
-- evaluator.
liftEvalI :: Functor f => (forall x. instr x -> f x)  -> Yoneda instr a -> f a
liftEvalI evalI (Yoneda f i) = fmap f (evalI i) 

liftInstr :: instr a -> Yoneda instr a
liftInstr = liftYoneda

