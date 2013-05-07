{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances #-}

-- | 'ProgramAlt' example: simple applicative parsers
module Parser where

import Control.Applicative
import Control.Alternative.Operational
import Data.Functor.Compose (Compose(..))
import Data.Maybe (listToMaybe)

---------------------------------------------------------------
---------------------------------------------------------------
--
-- Parser combinators
--

data ParserI a where
    Symbol :: Char -> ParserI Char

char :: Operational ParserI f => Char -> f Char
char = singleton . Symbol

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
      eval Empty _ = empty
      eval (p :<|> k) str = eval p str <|> eval k str

-- | Example parser.
parens :: ProgramAlt ParserI ()
parens = char' '(' *> optional parens *> char' ')'
    where char' c = char c *> pure ()



-- | Alternative implementation: interpret a parser program
-- denotationally, by evaluating each 'ParserI' instruction to an
-- 'Alternative' action.
runParser'  :: forall a. ProgramAlt ParserI a -> String -> Maybe a
runParser' = fmap listToMaybe . getCompose . unParserD . interpretAlt evalI
    where evalI :: forall x. ParserI x -> ParserD x
          evalI (Symbol c) = 
              ParserD . Compose $ \(x:xs) -> if c == x then pure c else empty

-- | Parser denotations.  
--
-- 'Compose' has an @(Alternative f, Applicative g) => Alternative
-- (Compose f g)@ instance that overlaps with our desired
-- 'Alternative' instance, so this needs to be a @newtype@.
newtype ParserD a = ParserD { unParserD :: (Compose ((->) String) [] a)}
    deriving (Functor, Applicative)

instance Alternative ParserD where
    empty = ParserD (Compose (pure empty))
    ParserD (Compose a) <|> ParserD (Compose b) =
        ParserD . Compose $ liftA2 (<|>) a b

