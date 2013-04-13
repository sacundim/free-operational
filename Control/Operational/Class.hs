module Control.Operational.Class 
    ( Operational(..)
    ) where

-- | The class of operational programs.  
class Operational p where
    -- | Make a program out of an instruction.
    singleton :: instr a -> p instr a
