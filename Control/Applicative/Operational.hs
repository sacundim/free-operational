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
    ) where

import Control.Applicative
import Control.Applicative.Free (Ap, runAp, liftAp)
import qualified Control.Applicative.Free as Free
import Data.Functor.Yoneda.Contravariant (Yoneda(..))


-- | An 'Applicative' program over instruction set @instr@.  This is
-- modeled after the 'Program' type from @Operational@
-- (<http://hackage.haskell.org/package/operational>), but this one is
-- an 'Applicative', not a 'Monad'.  This makes it less powerful, but
-- in exchange for the sacrificed power, 'ProgramA' is suceptible to
-- stronger static analysis.
--
-- For examples of this (though applied to free applicatives), see:
--
-- * <http://gergo.erdi.hu/blog/2012-12-01-static_analysis_with_applicatives/>
-- 
-- * <http://paolocapriotti.com/blog/2013/04/03/free-applicative-functors/>
newtype ProgramA instr a = 
    ProgramA { -- | Interpret a 'ProgramA' as a free applicative ('Ap').
               toAp :: Ap (Yoneda instr) a 
             } deriving (Functor, Applicative)

-- | Evaluate a 'ProgramA' by interpreting each instruction as an
-- 'Applicative' action. Example @Reader@ implementation:
--
-- > type Reader r a = ProgramA (ReaderI r) a
-- >
-- > data ReaderI r a where
-- >     Ask :: ReaderI r r
-- > 
-- > ask :: Reader r r
-- > ask = singleton Ask
-- > 
-- > runReader :: forall r a. Reader r a -> r -> a
-- > runReader = interpretA evalI
-- >     where evalI :: forall a. ReaderI r a -> r -> a
-- >           evalI Ask = id
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


-- | A friendly concrete tree view type for 'ProgramA'.  Unlike the
-- ':>>=' constructor in the 'ProgramView' type of
-- "Control.Monad.Operational", whose second data member is a function
-- that consumes an instruction result to generate the rest of the
-- program, our ':<**>' constructor exposes the rest of program
-- immediately.
--
-- Note that the 'ProgramViewA' type normalizes the program into a
-- different ordering and bracketing than the applicative '<*>'
-- operator does.  The ':<**>' constructor is instead an analogue of
-- @'<**>' :: Applicative f => f a -> f (a -> b) -> f b@ from
-- "Control.Applicative", so you get a list-like structure with
-- instructions as the elements and 'Pure' as the terminator.  The
-- instructions appear in the same order that their effects are
-- supposed to happen.
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
-- > count :: ProgramA FileSystemI a -> Int
-- > count = count' . viewA
-- >     where count' :: forall x. ProgramViewA FileSystemI x -> Int
-- >           count' (Pure _)   = 0
-- >           count' (_ :<**> k) = succ (count' k)
--
-- You can also use the 'ProgramViewA' to interpret the program, in
-- the style of the @operational@ package.  Example implementation of
-- a simple terminal language in this style:
--
-- > data TermI a where
-- >     Say :: String -> TermI ()
-- >     Get :: TermI String
-- > 
-- > say :: String -> ProgramA TermI ()
-- > say = singleton . Say
-- > 
-- > get :: ProgramA TermI String
-- > get = singleton Get
-- > 
-- > prompt :: String -> ProgramA TermI String
-- > prompt str = say str *> get
-- > 
-- > runTerm :: ProgramA TermI a -> IO a
-- > runTerm = eval . viewA
-- >     where eval :: forall x. ProgramViewA TermI x -> IO x
-- >           eval (Pure a) = pure a
-- >           eval (Say str :<**> k) = putStr str <**> eval k
-- >           eval (Get :<**> k)     = getLine    <**> eval k 
--
-- But as a general rule, 'interpretA' produces shorter, less
-- repetitive interpreters:
-- > runTerm :: ProgramA TermI a -> IO a
-- > runTerm = interpretA evalI
-- >     where evalI :: forall x. TermI x -> IO x
-- >           evalI (Say str)   = putStr str
-- >           evalI Get         = getLine
--
data ProgramViewA instr a where
    Pure   :: a -> ProgramViewA instr a
    (:<**>) :: instr a
            -> ProgramViewA instr (a -> b) 
            -> ProgramViewA instr b

infixl 4 :<**>

-- | Materialize a 'ProgramA' as a concrete tree.  Note that
-- 'ProgramA'\'s 'Functor' and 'Applicative' instances normalize their
-- programs, so the view term doesn't have to look like the code that
-- created it.
viewA :: ProgramA instr a -> ProgramViewA instr a
viewA = viewA' . toAp

viewA' :: Ap (Yoneda instr) a -> ProgramViewA instr a
viewA' (Free.Pure a) = Pure a
viewA' (Free.Ap (Yoneda f i) next) = i :<**> viewA' (fmap (.f) next)
