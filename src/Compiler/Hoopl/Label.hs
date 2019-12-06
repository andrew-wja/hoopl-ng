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

import Compiler.Hoopl.Collections
import Compiler.Hoopl.Unique
#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
#endif
import Data.Filtrable
import Data.Map.Class

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

newtype LabelSet = LS UniqueSet deriving (Eq, Ord, Show)

instance IsSet LabelSet where
  type ElemOf LabelSet = Label

  setNull (LS s) = setNull s
  setSize (LS s) = setSize s
  setMember (Label k) (LS s) = setMember k s

  setEmpty = LS setEmpty
  setSingleton (Label k) = LS (setSingleton k)
  setInsert (Label k) (LS s) = LS (setInsert k s)
  setDelete (Label k) (LS s) = LS (setDelete k s)

  setUnion (LS x) (LS y) = LS (setUnion x y)
  setDifference (LS x) (LS y) = LS (setDifference x y)
  setIntersection (LS x) (LS y) = LS (setIntersection x y)
  setIsSubsetOf (LS x) (LS y) = setIsSubsetOf x y

  setFold k z (LS s) = setFold (k . uniqueToLbl) z s

  setElems (LS s) = map uniqueToLbl (setElems s)
  setFromList ks = LS (setFromList (map lblToUnique ks))

-----------------------------------------------------------------------------
-- LabelMap

newtype LabelMap v = LM { unLM :: UniqueMap v }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Filtrable LabelMap where
  mapMaybe f = LM . mapMaybe f . unLM

instance StaticMap LabelMap where
  type Key LabelMap = Label
  traverseWithKey f = fmap LM . traverseWithKey (f . Label) . unLM
  adjustA f (Label k) = fmap LM . adjustA f k . unLM

instance Map LabelMap where
  empty = LM empty
  alterF f (Label k) = fmap LM . alterF f k . unLM
  mergeA f = (fmap . fmap) LM . \ (LM a) (LM b) -> mergeA (f . Label) a b
  mapMaybeWithKeyA f = fmap LM . mapMaybeWithKeyA (f . Label) . unLM

-----------------------------------------------------------------------------
-- FactBase

type FactBase f = LabelMap f

noFacts :: FactBase f
noFacts = empty

lookupFact :: Label -> FactBase f -> Maybe f
lookupFact = flip (!?)
