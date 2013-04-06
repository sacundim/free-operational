A reconstruction of Heinrich Apfelmus's [`operational`](http://hackage.haskell.org/package/operational) package, but:

1. Built with free monads (from [`free`](http://hackage.haskell.org/package/free)), and

2. with additional Applicative, Alternative and MonadPlus variants

Example: `Applicative` version of `Reader`:

    {-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

    import Control.Applicative.Operational

    type Reader r a = ProgramA (ReaderI r) a

    data ReaderI r a where
        Ask :: ReaderI r r

    ask :: Reader r r
    ask = singleton Ask

    runReader :: forall r a. Reader r a -> r -> a
    runReader = interpretA evalI
        where evalI :: forall x. ReaderI r x -> r -> x
              evalI Ask = id

Example: count how many times 'ask' is used in an applicative `Reader`
program:

    countAsk :: forall r a. Reader r a -> Int
    countAsk = count . viewA
        where count :: forall x. ProgramViewA (ReaderI r) x -> Int
              count (Pure _) = 0
              count (Instr Ask) = 1
              count (l :<*> r) = count l + count r
