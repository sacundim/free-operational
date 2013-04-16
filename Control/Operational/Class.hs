{-# LANGUAGE RankNTypes, TypeFamilies, KindSignatures, ConstraintKinds #-}

module Control.Operational.Class 
    ( Operational(..)
    ) where

import GHC.Exts (Constraint)

-- | The class of operational programs.  
class Operational p where
    type Semantics :: (* -> *) -> Constraint

    type View :: (* -> *) -> * -> *

    -- | Make a program out of an instruction.
    singleton :: instr a -> p instr a

    -- | Interpret a program.
    interpret :: Semantics f =>
                 (forall x. instr x -> f x)
              -> p instr a -> f a

    view :: p instr a -> View instr a

