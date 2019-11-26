{- Baseclasses for Map-like and Set-like collections inspired by containers. -}

{-# LANGUAGE CPP, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Collections ( IsSet(..)
                                  , setInsertList, setDeleteList, setUnions
                                  , IsMap(..)
                                  , mapInsertList, mapDeleteList, mapUnions
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


class IsMap map where
  type KeyOf map

  mapNull :: map a -> Bool
  mapSize :: map a -> Int
  mapMember :: KeyOf map -> map a -> Bool
  mapLookup :: KeyOf map -> map a -> Maybe a
  mapFindWithDefault :: a -> KeyOf map -> map a -> a

  mapEmpty :: map a
  mapSingleton :: KeyOf map -> a -> map a
  mapInsert :: KeyOf map -> a -> map a -> map a
  mapInsertWith :: (a -> a -> a) -> KeyOf map -> a -> map a -> map a
  mapDelete :: KeyOf map -> map a -> map a

  mapUnion :: map a -> map a -> map a
  mapUnionWithKey :: (KeyOf map -> a -> a -> a) -> map a -> map a -> map a
  mapDifference :: map a -> map a -> map a
  mapIntersection :: map a -> map a -> map a
  mapIsSubmapOf :: Eq a => map a -> map a -> Bool

  mapMap :: (a -> b) -> map a -> map b
  mapMapWithKey :: (KeyOf map -> a -> b) -> map a -> map b
  mapFold :: (a -> b -> b) -> b -> map a -> b
  mapFoldWithKey :: (KeyOf map -> a -> b -> b) -> b -> map a -> b
  mapFilter :: (a -> Bool) -> map a -> map a

  mapElems :: map a -> [a]
  mapKeys :: map a -> [KeyOf map]
  mapToList :: map a -> [(KeyOf map, a)]
  mapFromList :: [(KeyOf map, a)] -> map a
  mapFromListWith :: (a -> a -> a) -> [(KeyOf map,a)] -> map a

-- Helper functions for IsMap class
mapInsertList :: IsMap map => [(KeyOf map, a)] -> map a -> map a
mapInsertList assocs map = foldl' (flip (uncurry mapInsert)) map assocs

mapDeleteList :: IsMap map => [KeyOf map] -> map a -> map a
mapDeleteList keys map = foldl' (flip mapDelete) map keys

mapUnions :: IsMap map => [map a] -> map a
mapUnions = foldl' mapUnion mapEmpty

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
