{-# LANGUAGE CPP, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Label
  ( Label
  , freshLabel
  , LabelSet, LabelMap
  , FactBase, noFacts, lookupFact

  , uniqueToLbl -- MkGraph and GHC use only
  , lblToUnique -- GHC use only
  )

where

import Compiler.Hoopl.Unique
#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
#endif
import Data.Filtrable as F
import Data.Map.Class as Map
import Data.Set.Class as Set

-----------------------------------------------------------------------------
--		Label
-----------------------------------------------------------------------------

newtype Label = Label { lblToUnique :: Unique }
  deriving (Eq, Ord)

uniqueToLbl :: Unique -> Label
uniqueToLbl = Label

instance Show Label where
  show (Label n) = "L" ++ show n

freshLabel :: UniqueMonad m => m Label
freshLabel = freshUnique >>= return . uniqueToLbl

-----------------------------------------------------------------------------
-- LabelSet

newtype LabelSet = LS { unLS :: UniqueSet } deriving (Eq, Ord, Show)

instance Set LabelSet where
    type Elem LabelSet = Label
    empty = LS Set.empty
    insert l = LS . Set.insert (lblToUnique l) . unLS
    delete l = LS . Set.delete (lblToUnique l) . unLS
    singleton = LS . Set.singleton . lblToUnique
    foldr f z = Set.foldr (f . uniqueToLbl) z . unLS

-----------------------------------------------------------------------------
-- LabelMap

newtype LabelMap v = LM { unLM :: UniqueMap v }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Filtrable LabelMap where
  mapMaybe f = LM . F.mapMaybe f . unLM

instance StaticMap LabelMap where
  type Key LabelMap = Label
  traverseWithKey f = fmap LM . traverseWithKey (f . Label) . unLM
  adjustA f (Label k) = fmap LM . adjustA f k . unLM

instance Map LabelMap where
  empty = LM Map.empty
  alterF f (Label k) = fmap LM . alterF f k . unLM
  mergeA f = (fmap . fmap) LM . \ (LM a) (LM b) -> mergeA (f . Label) a b
  mapMaybeWithKeyA f = fmap LM . mapMaybeWithKeyA (f . Label) . unLM

-----------------------------------------------------------------------------
-- FactBase

type FactBase f = LabelMap f

noFacts :: FactBase f
noFacts = Map.empty

lookupFact :: Label -> FactBase f -> Maybe f
lookupFact = flip (!?)
