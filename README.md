[![Build Status](https://secure.travis-ci.org/sacundim/free-operational.png)](http://travis-ci.org/sacundim/free-operational)

free-operational
================

A reconstruction of Heinrich Apfelmus's
[`operational`](http://hackage.haskell.org/package/operational)
package, but:

1. Built with free monads (using Edward Kmett's
   [`free`](http://hackage.haskell.org/package/free) and
   [`kan-extensions`](http://hackage.haskell.org/package/kan-extensions)
   packages).  All the program types in this package can be translated
   to the corresponding `free` types and back (using
   [`Data.Functor.Yoneda.Contravariant`](http://hackage.haskell.org/packages/archive/kan-extensions/latest/doc/html/Data-Functor-Yoneda-Contravariant.html))

2. `Applicative`, `Alternative` and `MonadPlus` variants of
   `operational`'s `Program` type.  The `Applicative` and
   `Alternative` program types, in particular, allow for easy static
   analysis.


Example: `Applicative` version of `Reader`
------------------------------------------

    {-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

    import Control.Applicative.Operational

    type Reader r a = ProgramAp (ReaderI r) a

    data ReaderI r a where
        Ask :: ReaderI r r

    ask :: Reader r r
    ask = singleton Ask

    runReader :: forall r a. Reader r a -> r -> a
    runReader = interpretAp evalI
        where evalI :: forall x. ReaderI r x -> r -> x
              evalI Ask = id

Static analysis example: count how many times `ask` is used in an
applicative `Reader` program.

    countAsk :: forall r a. Reader r a -> Int
    countAsk = count . viewAp
        where count :: forall x. ProgramViewAp (ReaderI r) x -> Int
              count (Pure _) = 0
              count (Ask :<**> k) = succ (count k)

Since this `Reader` language only has one instruction, we can cheat
and make this even shorter:

    countAsk :: forall r a. Reader r a -> Int
    countAsk = length . filter isAsk . instructions
        where isAsk (AnyInstr Ask) = True


Example: Toy `Alternative` parsers
----------------------------------

Simple `Alternative` parser combinators:

    {-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

    import Control.Applicative
    import Control.Alternative.Operational
    import Control.Monad
    import Control.Monad.Trans.State
    import Data.Functor.Compose (Compose(..))
    import Data.Traversable
    import Data.Maybe (listToMaybe)

    data ParserI a where
        Symbol :: Char -> ParserI Char
    
    char :: Operational ParserI f => Char -> f Char
    char = singleton . Symbol
    
    string :: (Operational ParserI f, Applicative f) => String -> f String
    string = traverse char
    
    oneOf :: (Operational ParserI f, Alternative f) => String -> f Char
    oneOf = foldr (<|>) empty . map char
    
    -- | Example parser: match parentheses and count depth.
    parens :: ProgramAlt ParserI Int
    parens = pure 0  <|>  fmap (+1) (char '(' *> parens <* char ')')

Example "syntactic" interpreter, pattern matching on the view type:

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

Example "denotational" interpreter:

    runParser' :: ProgramAlt ParserI a -> String -> Maybe a
    runParser' = (firstSuccess .) . runStateT . interpretAlt evalParserI
        where firstSuccess [] = Nothing
              firstSuccess ((a,""):_) = Just a
              firstSuccess (_:xs) = firstSuccess xs
    
    evalParserI :: ParserI a -> StateT String [] a
    evalParserI (Symbol c) = 
        do str <- get
           case str of
             x:xs | c == x -> put xs >> return c
             otherwise     -> mzero

Simple static analysis example: enumerate the strings accepted by a
(non-degenerate) parser.

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

Example, using `parens` from above:

    >>> take 5 $ enumerate parens
    ["","()","(())","((()))","(((())))"]

Another toy static analysis example: optimize a (non-degenerate)
parser by merging on common prefixes.

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

    >>> describe $ tokens example
    OneOf ['a' :> ('b' :> ('a' :> ('c' :> ('t' :> ('o' :> ('r' :> Ok)))))),
           OneOf ['a' :> ('b' :> ('a' :> ('c' :> ('u' :> ('s' :> Ok))))),
                  OneOf ['a' :> ('b' :> ('a' :> ('f' :> ('t' :> Ok)))),
                         OneOf ['a' :> ('b' :> ('a' :> ('i' :> ('s' :> ('a' :> ('n' :> ('c' :> ('e' :> Ok)))))))),
                                OneOf ['a' :> ('b' :> ('a' :> ('i' :> ('s' :> ('s' :> ('e' :> ('d' :> Ok))))))),
                                       'a' :> ('b' :> ('a' :> ('l' :> ('o' :> ('n' :> ('e' :> Ok))))))]]]]]

    >>> describe $ optimize (tokens example)
    'a' :> ('b' :> ('a' :> OneOf ['c' :> OneOf ['t' :> ('o' :> ('r' :> Ok)),
                                                'u' :> ('s' :> Ok)],
                                  OneOf ['f' :> ('t' :> Ok),
                                         OneOf ['i' :> ('s' :> OneOf ['a' :> ('n' :> ('c' :> ('e' :> Ok))),
                                                                      's' :> ('e' :> ('d' :> Ok))]),
                                                'l' :> ('o' :> ('n' :> ('e' :> Ok)))]]]))


Sums of instruction sets
------------------------

`Control.Operational.Instruction` reexports `Data.Functor.Sum`,
which is rather useful in the context of this library:

    import Control.Operational.Instruction
    
    -- | An alternative parser instruction set, and an evaluation.
    data StringI a where
        String :: String -> StringI String
    
    evalStringI :: StringI a -> StateT String [] a
    evalStringI (String "") = return ""
    evalStringI (String str) = 
        do str' <- get
           case str `stripPrefix` str' of
             Nothing -> mzero
             Just suffix -> put suffix >> return str
    
    -- | If we know how to interpret two instruction sets at the same
    -- type, we know how to interpret their union.
    runStringP :: ProgramAlt (Sum ParserI StringI) a
               -> String
               -> [(a, String)]
    runStringP = runStateT . interpretAlt (coproduct evalParserI evalStringI)


References
----------

1. http://stackoverflow.com/questions/14263363/is-operational-really-isomorphic-to-a-free-monad
2. http://www.reddit.com/r/haskell/comments/17a33g/free_functors_the_reason_free_and_operational_are/
3. http://gergo.erdi.hu/blog/2012-12-01-static_analysis_with_applicatives/
4. http://paolocapriotti.com/blog/2013/04/03/free-applicative-functors/
5. http://web.jaguarpaw.co.uk/~tom/blog/2012/09/09/towards-free-applicatives.html
