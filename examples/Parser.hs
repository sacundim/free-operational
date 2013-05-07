{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances #-}

-- | 'ProgramAlt' example: simple applicative parsers
module Parser where

import Control.Applicative
import Control.Alternative.Operational
import Control.Operational.Instruction
import Control.Monad
import Control.Monad.Trans.State
import Data.Functor.Compose
import Data.Traversable
import Data.Maybe (listToMaybe)
import Data.List (find, stripPrefix)

---------------------------------------------------------------
---------------------------------------------------------------
--
-- Parser combinators
--

data ParserI a where
    Symbol :: Char -> ParserI Char

char :: Operational ParserI f => Char -> f Char
char = singleton . Symbol

string :: (Operational ParserI f, Applicative f) => String -> f String
string = traverse char

oneOf :: (Operational ParserI f, Alternative f) => String -> f Char
oneOf = foldr (<|>) empty . map char


-- | Interpret a parser program syntactically by pattern matching on
-- its view.
runParser :: ProgramAlt ParserI a -> String -> Maybe a
runParser = fmap listToMaybe . eval . viewAlt
    where
      eval :: ProgramViewAlt ParserI a -> String -> [a]
      eval (Pure a) [] = pure a
      eval (Pure a) _  = empty
      eval (Symbol c :<**> k) [] = empty
      eval (Symbol c :<**> k) (x:xs) 
          | c == x    = pure c <**> eval k xs
          | otherwise = empty
      eval (Many ps) str = fmap asum (sequenceA (map eval ps)) str

asum :: Alternative f => [f a] -> f a
asum = foldr (<|>) empty

-- | This version is maybe more conventional than 'runParser'.
runConventional :: ProgramAlt ParserI a -> String -> Maybe a
runConventional = (succeed .) . eval . viewAlt
    where eval :: ProgramViewAlt ParserI a -> String -> [(a, String)]
          eval (Pure a) str = [(a, str)]
          eval (Symbol c :<**> k) "" = []
          eval (Symbol c :<**> k) (x:xs)
               | c == x = [(k' c, str') | (k', str') <- eval k xs]
               | otherwise = []
          eval (Many ps) str = asum $ map (flip eval str) ps



-- | Example parser: match parentheses and count depth.
parens :: ProgramAlt ParserI Int
parens = pure 0  <|>  fmap (+1) (char '(' *> parens <* char ')')


-- | Interpret a parser program denotationally, by evaluating each
-- 'ParserI' instruction to an 'Alternative' action.
runParser' :: ProgramAlt ParserI a -> String -> Maybe a
runParser' = (succeed .) . runStateT . interpretAlt evalParserI

succeed :: [(a, String)] -> Maybe a
succeed = fmap fst . find step
    where step (a, "") = True
          step (_, _)  = False


evalParserI :: ParserI a -> StateT String [] a
evalParserI (Symbol c) = 
    do str <- get
       case str of
         x:xs | c == x -> put xs >> return c
         otherwise     -> mzero


-- | Static analysis example: enumerate the strings accepted by a parser.
-- Not all parsers can be enumerate; for example, this one doesn't:
--
-- > -- diverges:
-- > let a = char 'a' *> a 
-- > in enumerate a  
-- 
-- The problem in this example is that there is no string such that
-- the parser accepts it in finitely many steps.
enumerate :: ProgramAlt ParserI a -> [String]
enumerate = go [showString ""] . viewAlt
    where
      go :: [ShowS] -> ProgramViewAlt ParserI a -> [String]
      go strs (Pure a) = map ($"") strs
      go strs (Symbol c :<**> k) = go (map (.(showChar c)) strs) k
      go strs (Many ps) = interleave $ map (go strs) ps

interleave :: [[a]] -> [a]
interleave = foldr interleave2 []
    where
      interleave2 :: [a] -> [a] -> [a]
      interleave2 [] ys = ys
      interleave2 (x:xs) ys = x : interleave2 ys xs


-- | Static analysis example: optimize a parser by merging shared
-- prefixes.  This works by reducing a parser to this normal form:
--
-- > 1. Pure a
-- > 2. Symbol c :<**> nf  -- nf is in normal form
-- > 3. Many [nf, ...]     -- nf is Pure _ or Symbol c :<**> nf'
--
-- In addition, we order the branches by Symbol order to ensure
-- merging.
optimize :: ProgramAlt ParserI a -> ProgramAlt ParserI a
optimize = compileAlt . merge . viewAlt

merge :: ProgramViewAlt ParserI a -> ProgramViewAlt ParserI a
merge p@(Pure _) = p
merge (Symbol a :<**> k) = Symbol a :<**> merge k
merge (Many ps) = Many (mergeMany ps)

mergeMany :: [ProgramViewAlt ParserI a] -> [ProgramViewAlt ParserI a]
mergeMany = foldr step [] . map merge
    where step (Pure a) ps = Pure a : ps
          step (Symbol a :<**> l) ((Symbol b :<**> r) : ps) =
               case a `compare` b of
                 EQ -> (Symbol a :<**> Many (mergeMany [l, r])) : ps
                 LT -> (Symbol a :<**> l) : (Symbol b :<**> r) : ps
                 GT -> (Symbol b :<**> r) : (Symbol a :<**> l) : ps
          step (Symbol a :<**> l) ps = (Symbol a :<**> l) : ps
          step (Many ps) ps' = mergeMany (mergeMany ps ++ ps')



tokens :: [String] -> ProgramAlt ParserI String 
tokens = asum . map string

example = ["abactor", "abacus", "abaft", "abaisance", "abaissed", "abalone"]

describe :: forall a. ProgramAlt ParserI a -> Description
describe = eval . viewAlt
    where eval :: forall x. ProgramViewAlt ParserI x -> Description
          eval (Pure _) = Ok
          eval (Symbol c :<**> k) = c :> (eval k)
          eval (Many ps) = OneOf (map eval ps)

data Description = Ok
                 | Char :> Description
                 | OneOf [Description] 
                   deriving Show


data StringI a where
    String :: String -> StringI String

evalStringI :: StringI a -> StateT String [] a
evalStringI (String "") = return ""
evalStringI (String str) = 
    do str' <- get
       case str `stripPrefix` str' of
         Nothing -> mzero
         Just suffix -> put suffix >> return str

runStringP :: ProgramAlt (Coproduct ParserI StringI) a
           -> String
           -> [(a, String)]
runStringP = runStateT . interpretAlt (coproduct evalParserI evalStringI)
