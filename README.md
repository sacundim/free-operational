A version of Heinrich Apfelmus's operational package, but:

1. Built with free monads, and
2. With Applicative, Alternative and MonadPlus programs

Example: `Applicative` Reader:

    {-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

    type Reader r a = ProgramA (ReaderI r) a

    data ReaderI r a where
        Ask :: ReaderI r r

    ask :: Reader r r
    ask = singleton Ask

    runReader :: forall r a. Reader r a -> r -> a
    runReader = interpretA evalI
        where evalI :: forall x. ReaderI r x -> r -> x
              evalI Ask = id
