{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.STM
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Software Transactional Memory: a modular composable concurrency
-- abstraction.  See
--
--  * /Composable memory transactions/, by Tim Harris, Simon Marlow, Simon
--    Peyton Jones, and Maurice Herlihy, in
--    /ACM Conference on Principles and Practice of Parallel Programming/ 2005.
--    <https://www.microsoft.com/en-us/research/publication/composable-memory-transactions/>
--
-- This module only defines the 'STM' monad; you probably want to
-- import "Control.Concurrent.STM" (which exports "Control.Monad.STM").
--
-- Note that invariant checking (namely the @always@ and @alwaysSucceeds@
-- functions) has been removed. See ticket [#14324](https://ghc.haskell.org/trac/ghc/ticket/14324) and
-- the [removal proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0011-deprecate-stm-invariants.rst).
-- Existing users are encouraged to encapsulate their STM operations in safe
-- abstractions which can perform the invariant checking without help from the
-- runtime system.

-----------------------------------------------------------------------------

module Control.Monad.STM (
        STM,
        atomically,
#ifdef __GLASGOW_HASKELL__
        retry,
        orElse,
        check,
#endif
        throwSTM,
        catchSTM
  ) where

#ifdef __GLASGOW_HASKELL__
#if ! (MIN_VERSION_base(4,3,0))
import GHC.Conc
import Control.Monad    ( MonadPlus(..) )
import Control.Exception
#else
import GHC.Conc
#endif
#if !MIN_VERSION_base(4,22,0)
import GHC.Exts
import Control.Monad.Fix
#endif
#else
import Control.Sequential.STM
#endif

#ifdef __GLASGOW_HASKELL__
#if ! (MIN_VERSION_base(4,3,0))
import Control.Applicative
import Control.Monad (ap)
#endif
#endif

#if !MIN_VERSION_base(4,17,0)
import Control.Monad (liftM2)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid (..))
#endif
#endif


#ifdef __GLASGOW_HASKELL__
#if ! (MIN_VERSION_base(4,3,0))
instance MonadPlus STM where
  mzero = retry
  mplus = orElse

instance Applicative STM where
  pure = return
  (<*>) = ap

instance Alternative STM where
  empty = retry
  (<|>) = orElse
#endif

-- | Check that the boolean condition is true and, if not, 'retry'.
--
-- In other words, @check b = unless b retry@.
--
-- @since 2.1.1
check :: Bool -> STM ()
check b = if b then return () else retry
#endif


#if !MIN_VERSION_base(4,22,0)
data STMret a = STMret (State# RealWorld) a

liftSTM :: STM a -> State# RealWorld -> STMret a
liftSTM (STM m) = \s -> case m s of (# s', r #) -> STMret s' r

#endif
#if !MIN_VERSION_base(4,22,0)
-- | @since 2.3
instance MonadFix STM where
  mfix k = STM $ \s ->
    let ans        = liftSTM (k r) s
        STMret _ r = ans
    in case ans of STMret s' x -> (# s', x #)
#endif

#if !MIN_VERSION_base(4,17,0)
instance Semigroup a => Semigroup (STM a) where
    (<>) = liftM2 (<>)

instance Monoid a => Monoid (STM a) where
    mempty = return mempty
#if !MIN_VERSION_base(4,13,0)
    mappend = liftM2 mappend
#endif
#endif
