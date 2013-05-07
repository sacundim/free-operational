{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Simple examples of "Control.Applicative.Operational"
module Example where

import Control.Applicative
import Control.Applicative.Operational

---------------------------------------------------------------
---------------------------------------------------------------
--
-- Reader
--

type Reader r a = ProgramAp (ReaderI r) a

data ReaderI r a where
    Ask :: ReaderI r r

ask :: Reader r r
ask = singleton Ask

-- | \"Denotational\" implementation of 'runReader'; given an
-- evaluation of each instruction as an 'Applicative' action,
-- 'interpretAp' lifts it into a 'ProgramAp' evaluation.
runReader :: forall r a. Reader r a -> r -> a
runReader = interpretAp evalI
    where evalI :: forall x. ReaderI r x -> r -> x
          evalI Ask = id

-- | \"Syntactic\" implementation of 'runReader'; pattern matching on
-- program's view.
runReader' :: forall r a. Reader r a -> r -> a
runReader' = eval . viewAp
    where eval :: forall x. ProgramViewAp (ReaderI r) x -> r -> x
          eval (Pure a) = pure a
          eval (Ask :<**> k) = id <**> eval k

-- | Static analysis example: count how many uses of 'ask' in the
-- program.
countAsk :: forall r a. Reader r a -> Int
countAsk = count . viewAp
    where count :: forall x. ProgramViewAp (ReaderI r) x -> Int
          count (Pure _) = 0
          count (Ask :<**> k) = succ (count k)


-- | Static analysis example: count how many uses of 'ask' in the
-- program.
countAsk' :: forall r a. Reader r a -> Int
countAsk' = length . filter isAsk . instructions
    where isAsk (AnyInstr Ask) = True



---------------------------------------------------------------
---------------------------------------------------------------
--
-- FileSystem language (from Capriotti and Kaposi, 2013)
--


data FileSystemI a where
    Read  :: FilePath -> FileSystemI String 
    Write :: FilePath -> String -> FileSystemI ()

-- | Count how many file accesses a program does.
count :: ProgramAp FileSystemI a -> Int
count = length . instructions

-- | Same thing, but using 'viewAp' directly.
count' :: ProgramAp FileSystemI a -> Int
count' = count'' . viewAp
    where count'' :: forall x. ProgramViewAp FileSystemI x -> Int
          count'' (Pure _)   = 0
          count'' (_ :<**> k) = succ (count'' k)

countRead :: ProgramAp FileSystemI a -> Int
countRead = length . filter isRead . instructions
    where isRead (AnyInstr (Read _)) = True
          isRead (AnyInstr _) = False


---------------------------------------------------------------
---------------------------------------------------------------
--
-- Terminal language
--

data TermI a where
    Say :: String -> TermI ()
    Get :: TermI String

instance Show (TermI a) where
    show (Say str) = "Say " ++ show str
    show Get = "Get"

say :: String -> ProgramAp TermI ()
say = singleton . Say

get :: ProgramAp TermI String
get = singleton Get

prompt :: String -> ProgramAp TermI String
prompt str = say str *> get

runTerm :: ProgramAp TermI a -> IO a
runTerm = interpretAp evalI
    where evalI :: forall x. TermI x -> IO x
          evalI (Say str)   = putStr str
          evalI Get         = getLine

runTerm' :: ProgramAp TermI a -> IO a
runTerm' = eval . viewAp
    where eval :: forall x. ProgramViewAp TermI x -> IO x
          eval (Pure a) = pure a
          eval (Say str :<**> k) = putStr str <**> eval k
          eval (Get :<**> k)     = getLine    <**> eval k 

-- This is useful to quickly check that the first action does indeed
-- run before the second one.
termOrder :: ProgramAp TermI (String, String)
termOrder = (,) <$> prompt "First question: " <*> prompt "Second question: "
