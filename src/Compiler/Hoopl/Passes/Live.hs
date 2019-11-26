{-# LANGUAGE CPP, ScopedTypeVariables, RankNTypes, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Passes.Live 
  ( NodeWithVars(..), AssignmentNode(..)
  , liveLattice, liveness, deadAssignmentElim
  ) 
where

import Control.Monad (guard)
import Data.Monoid (Endo  (..))
import Data.Functor.Const (Const (..))

import Compiler.Hoopl

class (IsSet (VarSet n), ElemOf (VarSet n) ~ Var n, HooplNode n) => NodeWithVars n where
    type Var    n :: *
    -- ^ Variable or machine register.  Unequal variables don't alias.
    type VarSet n :: *
    foldVarsUsed :: ∀ e x a . (Var n -> a -> a) -> n e x -> a -> a
    foldVarsDefd :: ∀ e x a . (Var n -> a -> a) -> n e x -> a -> a
    varsUsed :: ∀ e x . n e x -> VarSet n
    varsDefd :: ∀ e x . n e x -> VarSet n
    killsAllVars :: ∀ e x . n e x -> Bool

    varsUsed = flip (foldVarsUsed setInsert) setEmpty
    varsDefd = flip (foldVarsDefd setInsert) setEmpty
    foldVarsUsed f = flip (setFold f) . varsUsed
    foldVarsDefd f = flip (setFold f) . varsDefd

class NodeWithVars n => AssignmentNode n where
    isVarAssign :: n O O -> Maybe (VarSet n) 

type Live n = WithTop (VarSet n)

liveLattice :: NodeWithVars n => Const (DataflowLattice (Live n)) n
liveLattice = Const $ addTop $ DataflowLattice
  { fact_name       = "Live variables"
  , fact_bot        = setEmpty
  , fact_join       = \ _ (OldFact old) (NewFact new) ->
        let x = setUnion new old in (changeIf (setSize x > setSize old), x)
  }

liveness :: NodeWithVars n => BwdTransfer n (VarSet n)
liveness = mkBTransfer3 gen_kill gen_kill (\ l -> gen_kill l . setUnions . successorFacts l)

gen_kill :: NodeWithVars n => n e x -> VarSet n -> VarSet n
gen_kill = appEndo . mconcat [gen, kill, mconcat . (Endo (pure setEmpty) <$) . guard . killsAllVars]


-- | The transfer equations use the traditional 'gen' and 'kill'
-- notations, which should be familiar from the dragon book.
gen, kill :: NodeWithVars n => n e x -> Endo (VarSet n)
gen  = Endo . foldVarsUsed setInsert
kill = Endo . foldVarsDefd setDelete

deadAssignmentElim :: ∀ m n . (FuelMonad m, AssignmentNode n) => BwdRewrite m n (VarSet n)
deadAssignmentElim = mkBRewrite3 goCO goOO goOC where
    goCO _ _ = pure Nothing
    goOC _ _ = pure Nothing

    goOO :: n O O -> VarSet n -> m (Maybe (Graph n O O))
    goOO n live = pure [emptyGraph | xs <- isVarAssign n, setNull (setIntersection xs live)]
