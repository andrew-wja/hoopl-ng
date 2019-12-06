{- Baseclasses for Map-like and Set-like collections inspired by containers. -}

{-# LANGUAGE CPP, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Collections ( IsSet(..)
                                  , setInsertList, setDeleteList, setUnions
                                  ) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (foldl', foldl1')
import Data.Set (Set)
import qualified Data.Set as Set

class IsSet set where
  type ElemOf set

  setNull :: set -> Bool
  setSize :: set -> Int
  setMember :: ElemOf set -> set -> Bool

  setEmpty :: set
  setSingleton :: ElemOf set -> set
  setInsert :: ElemOf set -> set -> set
  setDelete :: ElemOf set -> set -> set

  setUnion :: set -> set -> set
  setDifference :: set -> set -> set
  setIntersection :: set -> set -> set
  setIsSubsetOf :: set -> set -> Bool

  setFold :: (ElemOf set -> b -> b) -> b -> set -> b

  setElems :: set -> [ElemOf set]
  setFromList :: [ElemOf set] -> set

-- Helper functions for IsSet class
setInsertList :: IsSet set => [ElemOf set] -> set -> set
setInsertList keys set = foldl' (flip setInsert) set keys

setDeleteList :: IsSet set => [ElemOf set] -> set -> set
setDeleteList keys set = foldl' (flip setDelete) set keys

setUnions :: IsSet set => [set] -> set
setUnions [] = setEmpty
setUnions sets = foldl1' setUnion sets

instance Ord a => IsSet (Set a) where
    type ElemOf (Set a) = a
    setNull = Set.null
    setSize = Set.size
    setMember = Set.member
    setEmpty = Set.empty
    setSingleton = Set.singleton
    setInsert = Set.insert
    setDelete = Set.delete
    setUnion = Set.union
    setDifference = Set.difference
    setIntersection = Set.intersection
    setIsSubsetOf = Set.isSubsetOf
    setFold = Set.foldr
    setElems = Set.elems
    setFromList = Set.fromList

instance IsSet IntSet where
    type ElemOf IntSet = Int
    setNull = IS.null
    setSize = IS.size
    setMember = IS.member
    setEmpty = IS.empty
    setSingleton = IS.singleton
    setInsert = IS.insert
    setDelete = IS.delete
    setUnion = IS.union
    setDifference = IS.difference
    setIntersection = IS.intersection
    setIsSubsetOf = IS.isSubsetOf
    setFold = IS.foldr
    setElems = IS.elems
    setFromList = IS.fromList
