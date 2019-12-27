{-# LANGUAGE CPP, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif


module Compiler.Hoopl.Unique
  ( Unique, intToUnique
  , UniqueSet, UniqueMap
  , UniqueMonad(..)
  , SimpleUniqueMonad, runSimpleUniqueMonad
  , UniqueMonadT, runUniqueMonadT

  , uniqueToInt -- exposed through GHC module only!
  )

where

import Control.Monad.Trans.State
import Data.Functor.Identity (Identity (..))
import qualified Data.IntMap as M
import qualified Data.IntSet as S

#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
#endif

-----------------------------------------------------------------------------
--		Unique
-----------------------------------------------------------------------------

type Unique = Int

uniqueToInt :: Unique -> Int
uniqueToInt = id

intToUnique :: Int -> Unique
intToUnique = id

-----------------------------------------------------------------------------
-- UniqueSet

type UniqueSet = S.IntSet

-----------------------------------------------------------------------------
-- UniqueMap

type UniqueMap = M.IntMap

----------------------------------------------------------------
-- Monads

class Monad m => UniqueMonad m where
  freshUnique :: m Unique

type SimpleUniqueMonad = UniqueMonadT Identity

runSimpleUniqueMonad :: SimpleUniqueMonad a -> a
runSimpleUniqueMonad = runIdentity . runUniqueMonadT

----------------------------------------------------------------

newtype UniqueMonadT m a = UMT { unUMT :: [Unique] -> m (a, [Unique]) }
  deriving (Functor)
  deriving (Applicative, Monad) via (StateT [Unique] m)

instance Monad m => UniqueMonad (UniqueMonadT m) where
  freshUnique = UMT $ \ case
      (u:us) -> pure (u, us)
      _ -> error "Unique.freshUnique(UniqueMonadT): empty list"

runUniqueMonadT :: Functor m => UniqueMonadT m a -> m a
runUniqueMonadT m = fst <$> unUMT m allUniques

allUniques :: [Unique]
allUniques = [1..]
