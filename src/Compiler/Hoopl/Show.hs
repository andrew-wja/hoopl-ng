{-# LANGUAGE CPP, RankNTypes, GADTs, ScopedTypeVariables, FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Show 
  ( showGraph, showFactBase, Showing
  )
where

import Data.Foldable (toList)
import Data.Map.Class

import Compiler.Hoopl.Block (Block (..), MaybeO (..))
import Compiler.Hoopl.Graph
import Compiler.Hoopl.Label

--------------------------------------------------------------------------------
-- Prettyprinting
--------------------------------------------------------------------------------

type Showing n = forall e x . n e x -> String
 

showGraph :: forall n e x . Showing n -> Graph n e x -> String
showGraph node = \ case
    GNil -> ""
    GUnit block -> b block
    GMany g_entry g_blocks g_exit ->
        open b g_entry ++ concatMap b (toList g_blocks) ++ open b g_exit
  where
    b :: forall e x . Block n e x -> String
    b = \ case
        BlockCO l b1   -> node l ++ "\n" ++ b b1
        BlockCC l b1 n -> node l ++ "\n" ++ b b1 ++ node n ++ "\n"
        BlockOC   b1 n ->           b b1 ++ node n ++ "\n"
        BNil           -> ""
        BMiddle n      -> node n ++ "\n"
        BCat b1 b2     -> b b1   ++ b b2
        BSnoc b1 n     -> b b1   ++ node n ++ "\n"
        BCons n b1     -> node n ++ "\n" ++ b b1

open :: (a -> String) -> MaybeO z a -> String
open _ NothingO  = ""
open p (JustO n) = p n

showFactBase :: Show f => FactBase f -> String
showFactBase = show . foldrWithKey (curry (:)) []
