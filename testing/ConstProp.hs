{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module ConstProp (ConstFact, constLattice, initFact, varHasLit, constProp) where

import Control.Monad
import qualified Data.Map as Map
import Data.Map.Class

import Compiler.Hoopl hiding (empty, fromList)
import IR
import OptSupport

type Node = Insn -- for paper

-- ConstFact:
--   Not present in map => bottom
--   PElem v => variable has value v
--   Top     => variable's value is not constant
-- @ start cprop.tex
-- Type and definition of the lattice
type ConstFact = Map.Map Var (WithTop Lit)
constLattice :: DataflowLattice ConstFact
constLattice = DataflowLattice
 { fact_name = "Const var value"
 , fact_bot  = empty
 , fact_join = joinMaps (extendJoinDomain constFactAdd) }
 where
   constFactAdd _ (OldFact old) (NewFact new)
       = if new == old then (NoChange, PElem new)
         else               (SomeChange, Top)

-- @ end cprop.tex
-- Initially, we assume that all variable values are unknown.
initFact :: [Var] -> ConstFact
initFact vars = fromList $ [(v, Top) | v <- vars]

-- Only interesting semantic choice: values of variables are live across
-- a call site.
-- Note that we don't need a case for x := y, where y holds a constant.
-- We can write the simplest solution and rely on the interleaved optimization.
-- @ start cprop.tex
--------------------------------------------------
-- Analysis: variable equals a literal constant
varHasLit :: FwdTransfer Node ConstFact
varHasLit = mkFTransfer ft
 where
  ft :: Node e x -> ConstFact -> Fact x ConstFact
  ft (Label _)            f = f
  ft (Assign x (Lit k))   f = insert x (PElem k) f
  ft (Assign x _)         f = insert x Top f
  ft (Store _ _)          f = f
  ft (Branch l)           f = singleton l f
  ft (Cond (Var x) tl fl) f
      = mkFactBase constLattice
           [(tl, insert x (PElem (Bool True))  f),
            (fl, insert x (PElem (Bool False)) f)]
  ft (Cond _ tl fl) f
      = mkFactBase constLattice [(tl, f), (fl, f)]

-- @ end cprop.tex
  ft (Call vs _ _ bid)      f = singleton bid (foldl toTop f vs)
      where toTop f v = insert v Top f
  ft (Return _)             _ = empty

type MaybeChange a = a -> Maybe a
-- @ start cprop.tex
--------------------------------------------------
-- Rewriting: replace constant variables
constProp :: forall m. FuelMonad m => FwdRewrite m Node ConstFact
constProp = mkFRewrite cp
 where
   cp :: Node e x -> ConstFact -> m (Maybe (Graph Node e x))
   cp node f
     = return $ liftM insnToG $ mapVN (lookup f) node

   mapVN :: (Var  -> Maybe Expr) -> MaybeChange (Node e x)
   mapVN      = mapEN . mapEE . mapVE

   lookup :: ConstFact -> Var -> Maybe Expr
   lookup f x = case f !? x of
                  Just (PElem v) -> Just $ Lit v
                  _              -> Nothing
-- @ end cprop.tex
