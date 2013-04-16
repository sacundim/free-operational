{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Control.Operational.Class 
    ( Operational(..)
    ) where

-- | The class of operational programs.  
class Operational instr p | p -> instr where
    -- | Make a program out of an instruction.
    singleton :: instr a -> p a
