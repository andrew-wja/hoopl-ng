{-# LANGUAGE CPP, ScopedTypeVariables, RankNTypes, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Passes.Live 
  ( NodeWithVars(..), Var, AssignmentNode(..)
  , liveLattice, liveness, deadAssignmentElim
  ) 
where

import Control.Monad (guard)
import Data.Monoid (Endo  (..))
import Data.Functor.Const (Const (..))
import Data.Kind (Type)
import Data.Set.Class (Set (Elem))
import qualified Data.Set.Class as Set

import Compiler.Hoopl

type Var n = Elem (VarSet n)

class (Set (VarSet n), HooplNode n) => NodeWithVars n where
    type VarSet n :: Type
    -- ^ Variable or machine register.  Unequal variables don't alias.
    foldVarsUsed :: ∀ e x a . (Var n -> a -> a) -> n e x -> a -> a
    foldVarsDefd :: ∀ e x a . (Var n -> a -> a) -> n e x -> a -> a
    varsUsed :: ∀ e x . n e x -> VarSet n
    varsDefd :: ∀ e x . n e x -> VarSet n
    killsAllVars :: ∀ e x . n e x -> Bool

    varsUsed = flip (foldVarsUsed Set.insert) Set.empty
    varsDefd = flip (foldVarsDefd Set.insert) Set.empty
    foldVarsUsed f = flip (Set.foldr f) . varsUsed
    foldVarsDefd f = flip (Set.foldr f) . varsDefd

class NodeWithVars n => AssignmentNode n where
    isVarAssign :: n O O -> Maybe (VarSet n) 

type Live n = VarSet n

liveLattice :: NodeWithVars n => Const (DataflowLattice (Live n)) n
liveLattice = Const DataflowLattice
  { fact_name       = "Live variables"
  , fact_bot        = Set.empty
  , fact_join       = \ _ (OldFact old) (NewFact new) ->
        let x = Set.union new old in (changeIf (Set.size x > Set.size old), x)
  }

liveness :: NodeWithVars n => BwdTransfer n (VarSet n)
liveness = mkBTransfer3 gen_kill gen_kill (\ l -> gen_kill l . foldr Set.union Set.empty . successorFacts l)

gen_kill :: NodeWithVars n => n e x -> VarSet n -> VarSet n
gen_kill = appEndo . mconcat [gen, kill, mconcat . (Endo (pure Set.empty) <$) . guard . killsAllVars]


-- | The transfer equations use the traditional 'gen' and 'kill'
-- notations, which should be familiar from the dragon book.
gen, kill :: NodeWithVars n => n e x -> Endo (VarSet n)
gen  = Endo . foldVarsUsed Set.insert
kill = Endo . foldVarsDefd Set.delete

deadAssignmentElim :: ∀ m n . (FuelMonad m, AssignmentNode n) => BwdRewrite m n (VarSet n)
deadAssignmentElim = mkBRewrite3 goCO goOO goOC where
    goCO _ _ = pure Nothing
    goOC _ _ = pure Nothing

    goOO :: n O O -> VarSet n -> m (Maybe (Graph n O O))
    goOO n live = pure [emptyGraph | xs <- isVarAssign n, Set.null (Set.intersection xs live)]
