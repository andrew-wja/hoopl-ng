{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Live (liveLattice, liveness, deadAsstElim) where

import Data.Maybe
import qualified Data.Set as S

import Compiler.Hoopl
import IR
import OptSupport

type Live = S.Set Var
liveLattice :: DataflowLattice Live
liveLattice = DataflowLattice
  { fact_name       = "Live variables"
  , fact_bot        = S.empty
  , fact_extend     = add
  , fact_do_logging = False
  }
    where add _ (OldFact old) (NewFact new) = (ch, j)
            where
              j = new `S.union` old
              ch = if S.size j > S.size old then SomeChange else NoChange

liveness :: BwdTransfer Insn Live
liveness = mkBTransfer' live
  where
    live :: Insn e x -> Fact x Live -> Live
    live   (Label _)       f = f
    live n@(Assign x _)    f = addUses (S.delete x f) n
    live n@(Store _ _)     f = addUses f n
    live n@(Branch l)      f = addUses (fact f l) n
    live n@(Cond _ tl fl)  f = addUses (fact f tl `S.union` fact f fl) n
    live n@(Call vs _ _ l) f = addUses (fact f l `S.difference` S.fromList vs) n
    live n@(Return _)      _ = addUses (fact_bot liveLattice) n
    fact f l = fromMaybe S.empty $ lookupFact l f
    addUses = fold_EN (fold_EE addVar)
    addVar s (Var v) = S.insert v s
    addVar s _       = s
     
deadAsstElim :: forall m . Monad m => BwdRewrite m Insn Live
deadAsstElim = shallowBwdRw' d
  where
    d :: SimpleBwdRewrite' m Insn Live
    d (Assign x _) live = if x `S.member` live then Nothing
                                               else Just (return emptyGraph)
    d _ _ = Nothing
