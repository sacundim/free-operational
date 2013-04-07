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

Example: `Applicative` version of `Reader`.

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

Static analysis example: count how many times `ask` is used in an
applicative `Reader` program.

    countAsk :: forall r a. Reader r a -> Int
    countAsk = count . viewA
        where count :: forall x. ProgramViewA (ReaderI r) x -> Int
              count (Pure _) = 0
              count (Ask :<**> k) = succ (count k)

Or even shorter for this one-instruction language:

    countAsk :: forall r a. Reader r a -> Int
    countAsk = length . instructions



References:

1. http://stackoverflow.com/questions/14263363/is-operational-really-isomorphic-to-a-free-monad
2. http://www.reddit.com/r/haskell/comments/17a33g/free_functors_the_reason_free_and_operational_are/
3. http://gergo.erdi.hu/blog/2012-12-01-static_analysis_with_applicatives/
4. http://paolocapriotti.com/blog/2013/04/03/free-applicative-functors/
5. http://web.jaguarpaw.co.uk/~tom/blog/2012/09/09/towards-free-applicatives.html
