{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | 'Applicative' programs over an @operational@-style instruction
-- set, implemented on top of the 'Ap' free 'Applicative' type.
module Control.Applicative.Operational 
    ( ProgramA(..)
    , singleton
    , interpretA

    , ProgramViewA(..)
    , viewA
    , compileA
    ) where

import Control.Applicative
import Control.Applicative.Free (Ap, runAp, liftAp)
import Data.Functor.Yoneda.Contravariant (Yoneda(..))


-- | An 'Applicative' program over instruction set @instr@.  This is
-- modeled after the 'Program' type from "Control.Monad.Operational",
-- but since this one is not a 'Monad' it is less powerful.
--
-- In exchange for the sacrificed power, 'ProgramA' is easier to
-- analyze and materialize.  For example, whereas the 'view' and
-- 'viewT' functions in @operational@ can only expose one instruction
-- of the program at a time, our 'viewA' function can materialize the
-- whole 'ProgramA' as a tree in one shot, which makes it possible to
-- do things like static analysis.
newtype ProgramA instr a = 
    ProgramA { -- | Interpret a 'ProgramA' as a free applicative.
               toAp :: Ap (Yoneda instr) a }
             deriving (Functor, Applicative)

{-| Evaluate a 'ProgramA' by interpreting each instruction as an
'Applicative' action. Example @Reader@ implementation:

@
type Reader r a = ProgramA (ReaderI r) a

data ReaderI r a where
    Ask :: ReaderI r r

ask :: Reader r r
ask = singleton Ask

runReader :: forall r a. Reader r a -> r -> a
runReader = interpretA evalI
    where evalI :: forall a. ReaderI r a -> r -> a
          evalI Ask = id
@
-}
interpretA :: forall instr f a. Applicative f =>
              (forall x. instr x -> f x)
           -> ProgramA instr a 
           -> f a
interpretA evalI = runAp evalF . toAp
    where evalF :: forall x. Yoneda instr x -> f x
          evalF (Yoneda k i) = fmap k (evalI i)

-- | Turn an instruction into a 'ProgramA' action.
singleton :: instr a -> ProgramA instr a
singleton = ProgramA . liftAp . Yoneda id


{-| A friendly concrete tree view type for 'ProgramA'.  Unlike the
':>>=' constructor in the 'ProgramView' type of
"Control.Monad.Operational", whose second data member is a
continuation function to which to throw a result, our ':<*>'
constructor here exposes both subterms as 'ProgramViewA' values.  This
permits static analysis of a 'ProgramViewA'.

You can also use the 'ProgramViewA' to interpret the program, in the
style of the @operational@ package.  Example @Reader@ implementation
in this style:

@
type Reader r a = ProgramA (ReaderI r) a

data ReaderI r a where
    Ask :: ReaderI r r

ask :: Reader r r
ask = singleton Ask

runReader :: forall r a. Reader r a -> r -> a
runReader = eval . viewA
    where eval :: forall x. ProgramViewA (ReaderI r) x -> r -> x
          eval (Pure a) = pure a
          eval (Instr Ask) = id
          eval (ff :\<*\> fa) = eval ff \<*\> eval fa
@
-}
data ProgramViewA instr a where
    Pure   :: a -> ProgramViewA instr a
    Instr  :: instr a -> ProgramViewA instr a
    (:<*>) :: ProgramViewA instr (a -> b) 
           -> ProgramViewA instr a
           -> ProgramViewA instr b

infixl 4 :<*>

instance Functor (ProgramViewA instr) where
    fmap f (Pure a) = Pure (f a)
    fmap f (Instr i) = Pure f :<*> Instr i
    fmap f (ff :<*> fa) = ((f .) <$> ff) :<*> fa

instance Applicative (ProgramViewA instr) where
    pure = Pure
    (<*>) = (:<*>)


-- | Materialize a 'ProgramA' as a concrete tree.  Note that
-- 'ProgramA'\'s 'Functor' and 'Applicative' instances normalize their
-- programs, so the view term doesn't have to look like the code that
-- created it.
viewA :: ProgramA instr a -> ProgramViewA instr a
viewA = interpretA Instr


-- | The inverse of 'viewA'; turn a 'ProgramViewA' into a 'ProgramA'.
compileA :: ProgramViewA instr a -> ProgramA instr a
compileA (Pure a) = pure a
compileA (Instr i) = singleton i
compileA (ff :<*> fa) = compileA ff <*> compileA fa

