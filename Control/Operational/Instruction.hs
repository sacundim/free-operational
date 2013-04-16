{-# LANGUAGE Rank2Types #-}

module Control.Operational.Instruction 
    ( liftInstr
    , liftEvalI
    ) where

import Data.Functor.Yoneda.Contravariant

liftInstr :: instr a -> Yoneda instr a
liftInstr = liftYoneda

-- | Lift an operational instruction evaluator into a free 'Functor'
-- evaluator.
liftEvalI :: Functor f => (forall x. instr x -> f x)  -> Yoneda instr a -> f a
liftEvalI evalI (Yoneda f i) = fmap f (evalI i) 

