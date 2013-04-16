{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | 'Applicative' programs over an @operational@-style instruction
-- set, implemented on top of the 'Ap' free 'Applicative' type.
module Control.Applicative.Operational 
    ( module Control.Operational.Class
    , ProgramAp(..)
    , interpretAp
    , fromProgramAp

    , ProgramViewAp(..)
    , viewAp
    , foldProgramViewAp
    , instructions
    , AnyInstr(..)
    ) where

import Control.Applicative
import Control.Applicative.Free (Ap, runAp, liftAp)
import qualified Control.Applicative.Free as Free
import Control.Operational.Class
import Control.Operational.Instruction
import Data.Functor.Yoneda.Contravariant


-- | An 'Applicative' program over instruction set @instr@.  This is
-- modeled after the 'Program' type from @operational@
-- (<http://hackage.haskell.org/package/operational>), but this one is
-- an 'Applicative', not a 'Monad'.  This makes it less powerful, but
-- in exchange for the sacrificed power 'ProgramAp' is suceptible to
-- much stronger static analysis.
--
-- For examples of this (though applied to free applicatives), see:
--
-- * <http://gergo.erdi.hu/blog/2012-12-01-static_analysis_with_applicatives/>
-- 
-- * <http://paolocapriotti.com/blog/2013/04/03/free-applicative-functors/>
newtype ProgramAp instr a = 
    ProgramAp { -- | Interpret a 'ProgramAp' as a free applicative ('Ap').
               toAp :: Ap (Yoneda instr) a 
              } deriving (Functor, Applicative)

instance Operational instr (ProgramAp instr) where
    singleton = ProgramAp . liftAp . liftInstr

-- | Evaluate a 'ProgramAp' by interpreting each instruction as an
-- 'Applicative' action. Example @Reader@ implementation:
--
-- > type Reader r a = ProgramAp (ReaderI r) a
-- >
-- > data ReaderI r a where
-- >     Ask :: ReaderI r r
-- > 
-- > ask :: Reader r r
-- > ask = singleton Ask
-- > 
-- > runReader :: forall r a. Reader r a -> r -> a
-- > runReader = interpretAp evalI
-- >     where evalI :: forall a. ReaderI r a -> r -> a
-- >           evalI Ask = id
interpretAp :: forall instr f a.
               Applicative f =>
               (forall x. instr x -> f x)
            -> ProgramAp instr a 
            -> f a
interpretAp evalI = runAp (liftEvalI evalI) . toAp

-- | Lift a 'ProgramAp' into any other 'Operational' program type that
-- is at least as strong as 'Applicative'.
fromProgramAp :: (Operational instr (p instr), Applicative (p instr)) =>
                 ProgramAp instr a -> p instr a
fromProgramAp = interpretAp singleton


-- | A friendly concrete tree view type for 'ProgramAp'.  Unlike the
-- ':>>=' constructor in the 'ProgramView' type of
-- "Control.Monad.Operational", whose second data member is a function
-- that consumes an instruction result to generate the rest of the
-- program, our ':<**>' constructor exposes the rest of program
-- immediately.
--
-- Note that the 'ProgramViewAp' type normalizes the program into a
-- different ordering and bracketing than the applicative '<*>'
-- operator does.  The ':<**>' constructor is an analogue of @'<**>'
-- :: Applicative f => f a -> f (a -> b) -> f b@ from
-- "Control.Applicative".  The normalization means that you get a
-- list-like structure with instructions as the elements (in the same
-- order as their effects) and 'Pure' as the terminator.
--
-- A static analysis example, based on Capriotti and Kaposi (2013,
-- <http://paolocapriotti.com/blog/2013/04/03/free-applicative-functors/>):
--
-- > {-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
-- >
-- > import Control.Operational.Applicative
-- > 
-- > data FileSystemI a where
-- >     Read  :: FilePath -> FileSystemI String 
-- >     Write :: FilePath -> String -> FileSystemI ()
-- > 
-- > -- | Count how many file accesses a program does.
-- > count :: ProgramAp FileSystemI a -> Int
-- > count = count' . viewAp
-- >     where count' :: forall x. ProgramViewAp FileSystemI x -> Int
-- >           count' (Pure _)   = 0
-- >           count' (_ :<**> k) = succ (count' k)
-- 
-- Or actually, just this:
--
-- > count :: ProgramAp FileSystemI a -> Int
-- > count = length . instructions
--
-- You can also use the 'ProgramViewAp' to interpret the program, in
-- the style of the @operational@ package.  Example implementation of
-- a simple terminal language in this style:
--
-- > data TermI a where
-- >     Say :: String -> TermI ()
-- >     Get :: TermI String
-- > 
-- > say :: String -> ProgramAp TermI ()
-- > say = singleton . Say
-- > 
-- > get :: ProgramAp TermI String
-- > get = singleton Get
-- > 
-- > prompt :: String -> ProgramAp TermI String
-- > prompt str = say str *> get
-- > 
-- > runTerm :: ProgramAp TermI a -> IO a
-- > runTerm = eval . viewAp
-- >     where eval :: forall x. ProgramViewAp TermI x -> IO x
-- >           eval (Pure a) = pure a
-- >           eval (Say str :<**> k) = putStr str <**> eval k
-- >           eval (Get :<**> k)     = getLine    <**> eval k 
-- >
-- > example :: ProgramAp TermI (String, String)
-- > example = (,) <$> prompt "First question: " <*> prompt "Second question: "
-- > 
-- > -- example = Say "First question: " :<**> (Get :<**> (Say "Second question: " :<**> (Get :<**> Pure (\_ a _ b -> (a, b)))))
--
-- But as a general rule, 'interpretAp' makes for shorter, less
-- repetitive, fooler-proof interpreters:
--
-- > runTerm :: ProgramAp TermI a -> IO a
-- > runTerm = interpretAp evalI
-- >     where evalI :: forall x. TermI x -> IO x
-- >           evalI (Say str)   = putStr str
-- >           evalI Get         = getLine
--
data ProgramViewAp instr a where
    Pure   :: a -> ProgramViewAp instr a
    (:<**>) :: instr a
            -> ProgramViewAp instr (a -> b) 
            -> ProgramViewAp instr b

-- this is the same fixity as '<**>'; dunno why it's not infixr
infixl 4 :<**>  

-- | Materialize a 'ProgramAp' as a concrete tree.  Note that
-- 'ProgramAp''s 'Functor' and 'Applicative' instances normalize their
-- programs, so the view term will not have to look like the code that
-- created it.  Instructions however will appear in the order that
-- their effects should happen, from left to right.
viewAp :: ProgramAp instr a -> ProgramViewAp instr a
viewAp = viewAp' . toAp

viewAp' :: Ap (Yoneda instr) a -> ProgramViewAp instr a
viewAp' (Free.Pure a) = Pure a
viewAp' (Free.Ap (Yoneda f i) next) = i :<**> viewAp' (fmap (.f) next)


foldProgramViewAp :: (forall x. instr x -> r -> r) 
                 -> r
                 -> ProgramViewAp instr a
                 -> r
foldProgramViewAp k z (Pure _) = z
foldProgramViewAp k z (i :<**> is) = k i (foldProgramViewAp k z is)

instructions :: ProgramAp instr a -> [AnyInstr instr]
instructions = foldProgramViewAp (\i -> (AnyInstr i:)) [] . viewAp

data AnyInstr instr = forall a. AnyInstr (instr a)
