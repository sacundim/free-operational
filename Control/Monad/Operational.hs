{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A reconstruction of the @operational@ package in terms of the
-- 'FreeT' monad transformer.
--
-- This module is meant to be a drop-in replacement for its
-- counterpart in the @operational@ package.  Some of the
-- implementation choices reflect that:
--
-- * @'Program' instr@ and @'ProgramView' instr@ are type synonyms for
--   @'ProgramT' instr m@ and @'ProgramViewT' instr m@, just as in
--   @operational@.  If you don't care for that,
--   "Control.Monad.Operational.Simple" implements them directly in
--   terms of 'Free'.
--
-- * This module doesn't use the generic version of @singleton@ that
--   other modules in this package do, because I haven't figured out
--   (possibly through lack of trying) how to make the type class
--   compatible with the 'ProgramT' type.
module Control.Monad.Operational
    ( Program
    , ProgramView
    , view
    , interpretWithMonad

    , toFree
    , interpret

    , module Control.Monad.Trans.Operational

    , liftProgram
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Free (Free)
import qualified Control.Monad.Free as Free
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.Operational
import Data.Functor.Yoneda.Contravariant

type Program instr = ProgramT instr Identity

type ProgramView instr = ProgramViewT instr Identity

view :: Program instr a -> ProgramView instr a
view = runIdentity . viewT

-- | This function works the same way as its counterpart in the
-- @operational@ package, using 'view' and 'ProgramView'.
interpretWithMonad :: Monad m =>
                      (forall x. instr x -> m x)
                   -> Program instr a
                   -> m a
interpretWithMonad evalI = eval . view
    where eval (Return a) = return a
          eval (i :>>= k) = evalI i >>= interpretWithMonad evalI . k



-- | The 'Free' monad action for a 'Program'.
toFree :: Program instr a -> Free (Yoneda instr) a
toFree = freeT2Free . toFreeT

freeT2Free :: Functor f => FreeT f Identity a -> Free f a
freeT2Free = adjust . runIdentity . runFreeT
    where adjust (Pure a) = Free.Pure a
          adjust (Free fb) = Free.Free $ fmap freeT2Free fb

-- | Interpret a 'Program' by interpreting each instruction as a
-- monadic action.  Unlike 'interpretWithMonad', this soes not use
-- 'view' nor 'ProgramView'.
--
-- This function is not a drop-in replacement for 'interpretWithMonad'
-- because it has an extra @Functor m@ constraint.
interpret :: forall m instr a. (Functor m, Monad m) =>
             (forall x. instr x -> m x)
          -> Program instr a
          -> m a
interpret evalI = Free.retract . Free.hoistFree evalF . toFree
    where evalF :: forall x. Yoneda instr x -> m x
          evalF (Yoneda f i) = liftM f (evalI i)

liftProgram :: Monad m => Program instr a -> ProgramT instr m a
liftProgram = ProgramT . hoistFreeT (return . runIdentity) . toFreeT
