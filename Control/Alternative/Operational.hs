{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | @operational@-style 'Alternative' programs.  See
-- "Control.Applicative.Operational" for guidance on how to use this
-- module.
--
-- Example: simple applicative parsers:
--
-- > import Control.Applicative
-- > import Control.Alternative.Operational
-- > import Control.Alternative.Monad (void)
-- > import Data.Functor.Compose (Compose(..))
-- > import Data.Traversable
-- > import Data.Maybe (listToMaybe)
-- > data ParserI a where
-- >     Symbol :: Char -> ParserI Char
-- > 
-- > char :: Operational ParserI f => Char -> f Char
-- > char = singleton . Symbol
-- > 
-- > string :: (Operational ParserI f, Applicative f) => String -> f String
-- > string = traverse char
-- > 
-- > oneOf :: (Operational ParserI f, Alternative f) => String -> f Char
-- > oneOf = foldr (<|>) empty . map char
-- > 
-- > 
-- > -- | Example parser: match parentheses and count depth.
-- > parens :: ProgramAlt ParserI Int
-- > parens = pure 0  <|>  char '(' *> fmap (+1) parens <* char ')'
-- > 
-- > 
-- > -- | Interpret a parser program \"syntactically\" by pattern matching
-- > -- on its view.
-- > runParser :: ProgramAlt ParserI a -> String -> Maybe a
-- > runParser = fmap listToMaybe . eval . viewAlt
-- >     where
-- >       eval :: ProgramViewAlt ParserI a -> String -> [a]
-- >       eval (Pure a) [] = pure a
-- >       eval (Pure a) _  = empty
-- >       eval (Symbol c :<**> k) [] = empty
-- >       eval (Symbol c :<**> k) (x:xs) 
-- >           | c == x    = pure c <**> eval k xs
-- >           | otherwise = empty
-- >       eval (Many ps) str = fmap asum (sequenceA (map eval ps)) str
-- > 
-- > asum :: Alternative f => [f a] -> f a
-- > asum = foldr (<|>) empty
--
-- Alternatively, programs may be interpreted in a more denotational
-- style:
--
-- > runParser :: ProgramAlt ParserI a -> String -> Maybe a
-- > runParser = (firstSuccess .) . runStateT . interpretAlt evalParserI
-- >     where firstSuccess [] = Nothing
-- >           firstSuccess ((a,""):_) = Just a
-- >           firstSuccess (_:xs) = firstSuccess xs
-- > 
-- > evalParserI :: ParserI a -> StateT String [] a
-- > evalParserI (Symbol c) = 
-- >     do str <- get
-- >        case str of
-- >          x:xs | c == x -> put xs >> return c
-- >          otherwise     -> mzero
--
-- One of the big \"powers\" of 'ProgramAlt' is that it allows for
-- powerful static analysis of programs.  For example, we can
-- enumerate the strings accepted by a non-degenerate parser:
--
-- > -- | Static analysis example: enumerate the strings accepted by a parser.
-- > enumerate :: ProgramAlt ParserI a -> [String]
-- > enumerate = go [showString ""] . viewAlt
-- >     where
-- >       go :: [ShowS] -> ProgramViewAlt ParserI a -> [String]
-- >       go strs (Pure a) = map ($"") strs
-- >       go strs (Symbol c :<**> k) = go (map (.(showChar c)) strs) k
-- >       go strs (Many ps) = interleave $ map (go strs) ps
-- > 
-- > interleave :: [[a]] -> [a]
-- > interleave = foldr interleave2 []
-- >     where
-- >       interleave2 :: [a] -> [a] -> [a]
-- >       interleave2 [] ys = ys
-- >       interleave2 (x:xs) ys = x : interleave2 ys xs
--
-- >>> take 7 (enumerate parens)
-- ["","()","(())","((()))","(((())))","((((()))))","(((((())))))"]
-- 
-- (@enumerate@ isn't guaranteed to terminate or even produce WHNF for
-- all parsers; e.g., @let a = char 'a' *> a in enumerate a@
-- diverges.  But this parser doesn't accept any strings!)
--
-- Or we can optimize a (non-degenerate) parser by merging prefixes:
--
-- > optimize :: ProgramAlt ParserI a -> ProgramAlt ParserI a
-- > optimize = compileAlt . merge . viewAlt
-- > 
-- > merge :: ProgramViewAlt ParserI a -> ProgramViewAlt ParserI a
-- > merge p@(Pure _) = p
-- > merge (Symbol a :<**> k) = Symbol a :<**> merge k
-- > merge (Many ps) = Many (mergeMany ps)
-- > 
-- > mergeMany :: [ProgramViewAlt ParserI a] -> [ProgramViewAlt ParserI a]
-- > mergeMany = foldr step [] . map merge
-- >     where step (Pure a) ps = Pure a : ps
-- >           step (Symbol a :<**> l) ((Symbol b :<**> r) : ps) =
-- >                case a `compare` b of
-- >                  EQ -> (Symbol a :<**> Many (mergeMany [l, r])) : ps
-- >                  LT -> (Symbol a :<**> l) : (Symbol b :<**> r) : ps
-- >                  GT -> (Symbol b :<**> r) : (Symbol a :<**> l) : ps
-- >           step (Symbol a :<**> l) ps = (Symbol a :<**> l) : ps
-- >           step (Many ps) ps' = mergeMany (mergeMany ps ++ ps')
--
-- (Also not guaranteed to terminate on all cases; @let a = a <* char
-- 'a' in optimize a@ diverges, but that parser never terminates for
-- any string.)
--
-- Example of @optimize@:
--
-- > tokens :: [String] -> ProgramAlt ParserI String 
-- > tokens = asum . map string
-- > 
-- > example = [ "abactor", "abacus", "abaft", "abaisance", "abaissed", "abalone"
-- >           ]
-- > 
-- > describe :: forall a. ProgramAlt ParserI a -> Description
-- > describe = eval . viewAlt
-- >     where eval :: forall x. ProgramViewAlt ParserI x -> Description
-- >           eval (Pure _) = Ok
-- >           eval (Symbol c :<**> k) = c :> (eval k)
-- >           eval (Many ps) = OneOf (map eval ps)
-- > 
-- > data Description = Ok 
-- >                  | Char :> Description
-- >                  | OneOf [Description] 
-- >                    deriving Show
-- 
-- >>>  describe $ tokens example
-- OneOf ['a' :> ('b' :> ('a' :> ('c' :> ('t' :> ('o' :> ('r' :> Ok)))))),
--        OneOf ['a' :> ('b' :> ('a' :> ('c' :> ('u' :> ('s' :> Ok))))),
--               OneOf ['a' :> ('b' :> ('a' :> ('f' :> ('t' :> Ok)))),
--                      OneOf ['a' :> ('b' :> ('a' :> ('i' :> ('s' :> ('a' :> ('n' :> ('c' :> ('e' :> Ok)))))))),
--                             OneOf ['a' :> ('b' :> ('a' :> ('i' :> ('s' :> ('s' :> ('e' :> ('d' :> Ok))))))),
--                                    'a' :> ('b' :> ('a' :> ('l' :> ('o' :> ('n' :> ('e' :> Ok))))))]]]]]
-- >>> describe $ optimize (tokens example)
-- 'a' :> ('b' :> ('a' :> OneOf ['c' :> OneOf ['t' :> ('o' :> ('r' :> Ok)),'u' :> ('s' :> Ok)],
--                               OneOf ['f' :> ('t' :> Ok),
--                                      OneOf ['i' :> ('s' :> OneOf ['a' :> ('n' :> ('c' :> ('e' :> Ok))),
--                                                                   's' :> ('e' :> ('d' :> Ok))]),
--                                             'l' :> ('o' :> ('n' :> ('e' :> Ok)))]]]))
module Control.Alternative.Operational 
    ( module Control.Operational.Class
    , ProgramAlt(..)
    , interpretAlt
    , fromProgramAlt

    , ProgramViewAlt(..)
    , viewAlt
    , compileAlt
    , foldProgramViewAlt
    ) where

import Control.Applicative
import qualified Control.Alternative.Free as Free
import Control.Alternative.Free hiding (Pure)
import Control.Operational.Class
import Control.Operational.Instruction
import Data.Functor.Yoneda.Contravariant

newtype ProgramAlt instr a =
    ProgramAlt { -- | Interpret the program as a free 'Alternative' ('Alt').
                 toAlt :: Alt (Yoneda instr) a 
               } deriving (Functor, Applicative, Alternative)

instance Operational instr (ProgramAlt instr) where
    singleton = ProgramAlt . liftAlt . liftInstr

interpretAlt :: forall instr f a.
                Alternative f =>
               (forall x. instr x -> f x)
             -> ProgramAlt instr a 
             -> f a
interpretAlt evalI = runAlt (liftEvalI evalI) . toAlt

fromProgramAlt 
    :: (Operational instr f, Alternative f) => ProgramAlt instr a -> f a
fromProgramAlt = interpretAlt singleton

data ProgramViewAlt instr a where
    Pure    :: a -> ProgramViewAlt instr a
    (:<**>) :: instr a
            -> ProgramViewAlt instr (a -> b) 
            -> ProgramViewAlt instr b
    Many    :: [ProgramViewAlt instr a] -> ProgramViewAlt instr a

viewAlt :: ProgramAlt instr a -> ProgramViewAlt instr a
viewAlt = viewAlt' . toAlt

viewAlt' :: Alt (Yoneda instr) a -> ProgramViewAlt instr a
viewAlt' (Free.Pure a) = Pure a
viewAlt' (Free.Ap (Yoneda f i) next) = i :<**> viewAlt' (fmap (.f) next)
viewAlt' (Free.Alt xs) = Many $ map viewAlt' xs


compileAlt :: ProgramViewAlt instr a -> ProgramAlt instr a
compileAlt (Pure a) = pure a
compileAlt (i :<**> k) = singleton i <**> compileAlt k
compileAlt (Many xs) = foldr (<|>) empty $ map compileAlt xs

foldProgramViewAlt
    :: (forall x. instr x -> r -> r)  -- ^ ':<**>'
    -> r                              -- ^ 'Pure'
    -> ([r] -> r)                     -- ^ 'Many'
    -> ProgramViewAlt instr a
    -> r
foldProgramViewAlt k z m (Pure _) = z
foldProgramViewAlt k z m (i :<**> is) = k i (foldProgramViewAlt k z m is)
foldProgramViewAlt k z m (Many xs) = m (map subfold xs)
    where subfold = foldProgramViewAlt k z m
