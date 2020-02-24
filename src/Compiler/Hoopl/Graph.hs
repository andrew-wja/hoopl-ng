{-# LANGUAGE CPP, GADTs, TypeFamilies, ScopedTypeVariables,
    RankNTypes, TypeSynonymInstances,
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ < 701
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

module Compiler.Hoopl.Graph 
  (
    -- * Body
    Body, Body', emptyBody, bodyList, addBlock, bodyUnion

    -- * Graph
  , Graph, Graph'(..)
  , NonLocal(entryLabel, successors)
  , gShape

  -- ** Constructing graphs
  , bodyGraph
  , blockGraph
  , gUnitOO, gUnitOC, gUnitCO, gUnitCC
  , catGraphNodeOC, catGraphNodeOO
  , catNodeCOGraph, catNodeOOGraph

  -- ** Splicing graphs
  , splice, gSplice

  -- ** Maps
  , mapGraph, mapGraph3, mapGraphBlocks, mapGraph'
  , traverseGraph, traverseGraph3, traverseGraphBlocks, traverseGraph'

  -- ** Folds
  , foldGraphNodes, foldGraphNodes3

  -- ** Extracting Labels
  , labelsDefined, labelsUsed, externalEntryLabels

  -- ** Depth-first traversals
  , postorder_dfs, postorder_dfs_from, postorder_dfs_from_except
  , preorder_dfs, preorder_dfs_from_except
  , LabelsPtr(..)
  )
where

import Compiler.Hoopl.Block (Block (..), C, O, IndexedCO, MaybeO (..), Shape (..), mapBlock, traverseBlock, traverseBlock3, foldBlockNodesF3)
import qualified Compiler.Hoopl.Block as Block
import Compiler.Hoopl.Label

import Control.Applicative as AP (Applicative(..))
import Control.Categorical.Functor (NT (..))
import qualified Control.Categorical.Functor as C
import Control.Categorical.Monad (Kleisli (..))
import Control.Monad.Trans.State (State, evalState, state)
import Data.Functor.Identity (Identity (..))
import Data.Map.Class as Map
import qualified Data.Set.Class as Set

-- -----------------------------------------------------------------------------
-- Body

-- | A (possibly empty) collection of closed/closed blocks
type Body n = LabelMap (Block n C C)

-- | @Body@ abstracted over @block@
type Body' block n = LabelMap (block n C C)

emptyBody :: Body' block n
emptyBody = Map.empty

bodyUnion :: forall a . LabelMap a -> LabelMap a -> LabelMap a
bodyUnion = unionWith nodups
  where nodups l _ _ = error $ "duplicate blocks with label " ++ show l

bodyList :: Body' block n -> [(Label,block n C C)]
bodyList = foldrWithKey (curry (:)) []

addBlock :: NonLocal thing
         => thing C C -> LabelMap (thing C C)
         -> LabelMap (thing C C)
addBlock b body
  | Just _ <- body !? lbl = error $ "duplicate label " ++ show lbl ++ " in graph"
  | otherwise             = insert lbl b body
  where lbl = entryLabel b


-- ---------------------------------------------------------------------------
-- Graph

-- | A control-flow graph, which may take any of four shapes (O/O,
-- O/C, C/O, C/C).  A graph open at the entry has a single,
-- distinguished, anonymous entry point; if a graph is closed at the
-- entry, its entry point(s) are supplied by a context.
type Graph = Graph' Block

-- | @Graph'@ is abstracted over the block type, so that we can build
-- graphs of annotated blocks for example (Compiler.Hoopl.Dataflow
-- needs this).
data Graph' block n e x where
  GNil  :: Graph' block n O O
  GUnit :: block n O O -> Graph' block n O O
  GMany :: MaybeO e (block n O C)
        -> Body' block n
        -> MaybeO x (block n C O)
        -> Graph' block n e x

gShape :: Graph' block n e x -> (Shape e, Shape x)
gShape = \ case
    GNil -> (Open, Open)
    GUnit _ -> (Open, Open)
    GMany a _ b ->
        let f :: MaybeO x a -> Shape x
            f = \ case
                NothingO -> Closed
                JustO _ -> Open
        in (f a, f b)

instance (Applicative m, C.Functor (NT (NT (Kleisli (->) m))) (NT (NT (Kleisli (->) m))) block) => C.Functor (NT (NT (Kleisli (->) m))) (NT (NT (Kleisli (->) m))) (Graph' block) where
    map f' = NT (NT (Kleisli (traverseGraphBlocks (kleisli (nt (nt (C.map f')))))))

instance C.Functor (NT (NT (->))) (NT (NT (->))) block => C.Functor (NT (NT (->))) (NT (NT (->))) (Graph' block) where
    map f' = NT (NT (mapGraphBlocks (nt (nt (C.map f')))))


-------------------------------
-- | Gives access to the anchor points for
-- nonlocal edges as well as the edges themselves
class NonLocal thing where 
  entryLabel :: thing C x -> Label   -- ^ The label of a first node or block
  successors :: thing e C -> [Label] -- ^ Gives control-flow successors

instance NonLocal n => NonLocal (Block n) where
  entryLabel (BlockCO f _)   = entryLabel f
  entryLabel (BlockCC f _ _) = entryLabel f

  successors (BlockOC   _ n) = successors n
  successors (BlockCC _ _ n) = successors n


-- -----------------------------------------------------------------------------
-- Constructing graphs

bodyGraph :: Body n -> Graph n C C
bodyGraph b = GMany NothingO b NothingO

gUnitOO :: block n O O -> Graph' block n O O
gUnitOC :: block n O C -> Graph' block n O C
gUnitCO :: block n C O -> Graph' block n C O
gUnitCC :: NonLocal (block n) => block n C C -> Graph' block n C C
gUnitOO b = GUnit b
gUnitOC b = GMany (JustO b) emptyBody  NothingO
gUnitCO b = GMany NothingO  emptyBody (JustO b)
gUnitCC b = GMany NothingO (addBlock b emptyBody) NothingO


catGraphNodeOO ::               Graph n e O -> n O O -> Graph n e O
catGraphNodeOC :: NonLocal n => Graph n e O -> n O C -> Graph n e C
catNodeOOGraph ::               n O O -> Graph n O x -> Graph n O x
catNodeCOGraph :: NonLocal n => n C O -> Graph n O x -> Graph n C x

catGraphNodeOO GNil                     n = gUnitOO $ BMiddle n
catGraphNodeOO (GUnit b)                n = gUnitOO $ BSnoc b n
catGraphNodeOO (GMany e body (JustO (BlockCO f b))) n
  = GMany e body (JustO (BlockCO f (BSnoc b n)))

catGraphNodeOC GNil                     n = gUnitOC $ BlockOC BNil n
catGraphNodeOC (GUnit b)                n = gUnitOC $ BlockOC b n
catGraphNodeOC (GMany e body (JustO (BlockCO f x))) n
  = GMany e (addBlock (BlockCC f x n) body) NothingO

catNodeOOGraph n GNil                     = gUnitOO $ BMiddle n
catNodeOOGraph n (GUnit b)                = gUnitOO $ BCons n b
catNodeOOGraph n (GMany (JustO (BlockOC b l)) body x)
   = GMany (JustO (BlockOC (n `BCons` b) l)) body x

catNodeCOGraph f GNil                     = gUnitCO (BlockCO f BNil)
catNodeCOGraph f (GUnit b)                = gUnitCO (BlockCO f b)
catNodeCOGraph f (GMany (JustO (BlockOC b n)) body x)
  = GMany NothingO (addBlock (BlockCC f b n) body) x


blockGraph :: NonLocal n => Block n e x -> Graph n e x
blockGraph b@(BlockCO {}) = gUnitCO b
blockGraph b@(BlockOC {}) = gUnitOC b
blockGraph b@(BlockCC {}) = gUnitCC b
blockGraph   (BNil  {})   = GNil
blockGraph b@(BMiddle {}) = gUnitOO b
blockGraph b@(BCat {})    = gUnitOO b
blockGraph b@(BSnoc {})   = gUnitOO b
blockGraph b@(BCons {})   = gUnitOO b


-- -----------------------------------------------------------------------------
-- Splicing graphs

splice :: forall block n e a x . NonLocal (block n) =>
          (forall e x . block n e O -> block n O x -> block n e x)
       -> (Graph' block n e a -> Graph' block n a x -> Graph' block n e x)

splice bcat = sp
  where sp :: forall e a x .
              Graph' block n e a -> Graph' block n a x -> Graph' block n e x

        sp GNil g2 = g2
        sp g1 GNil = g1

        sp (GUnit b1) (GUnit b2) = {-# SCC "sp1" #-} GUnit $! b1 `bcat` b2

        sp (GUnit b) (GMany (JustO e) bs x) = {-# SCC "sp2" #-} GMany (JustO (b `bcat` e)) bs x

        sp (GMany e bs (JustO x)) (GUnit b2) = {-# SCC "sp3" #-} x `seq` GMany e bs (JustO x')
             where x' = x `bcat` b2

        sp (GMany e1 bs1 (JustO x1)) (GMany (JustO e2) b2 x2)
          = {-# SCC "sp4" #-} (GMany e1 $! b1 `bodyUnion` b2) x2
          where b1   = (addBlock $! x1 `bcat` e2) bs1

        sp (GMany e1 b1 NothingO) (GMany NothingO b2 x2)
          = {-# SCC "sp5" #-} (GMany e1 $! b1 `bodyUnion` b2) x2

#if __GLASGOW_HASKELL__ < 711
        sp _ _ = error "bogus GADT match failure"
#endif

gSplice :: NonLocal n => Graph n e a -> Graph n a x -> Graph n e x
gSplice = splice Block.append


-- -----------------------------------------------------------------------------
-- Mapping over graphs

-- | Maps over all nodes in a graph.
mapGraph :: (forall e x. n e x -> n' e x) -> Graph n e x -> Graph n' e x
mapGraph f = runIdentity . traverseGraph (Identity . f)

-- | Maps over all nodes in a graph.
mapGraph3 :: (n C O -> n' C O,
              n O O -> n' O O,
              n O C -> n' O C) -> Graph n e x -> Graph n' e x
mapGraph3 (fCO, fOO, fOC) = runIdentity . traverseGraph3 (Identity . fCO, Identity . fOO, Identity . fOC)

-- | Function 'mapGraphBlocks' enables a change of representation of blocks,
-- nodes, or both.  It lifts a polymorphic block transform into a polymorphic
-- graph transform.  When the block representation stabilizes, a similar
-- function should be provided for blocks.
mapGraphBlocks :: forall block n block' n' e x .
                  (forall e x . block n e x -> block' n' e x)
               -> (Graph' block n e x -> Graph' block' n' e x)
mapGraphBlocks f = runIdentity . traverseGraphBlocks (Identity . f)

mapGraph' :: NT (NT (->)) n n' -> NT (NT (->)) (Graph n) (Graph n')
mapGraph' f = NT (NT (mapGraphBlocks (mapBlock (nt (nt f)))))

traverseGraph :: Applicative p => (∀ e x . n e x -> p (n' e x)) -> Graph n e x -> p (Graph n' e x)
traverseGraph f = traverseGraphBlocks (traverseBlock f)

traverseGraph3 :: Applicative p => (n C O -> p (n' C O),
                                    n O O -> p (n' O O),
                                    n O C -> p (n' O C)) -> Graph n e x -> p (Graph n' e x)
traverseGraph3 f = traverseGraphBlocks (traverseBlock3 f)

traverseGraphBlocks :: ∀ block n block' n' e x p .
    Applicative p
 => (∀ e x . block n e x -> p (block' n' e x))
 -> Graph' block n e x -> p (Graph' block' n' e x)
traverseGraphBlocks f = go where
    go :: Graph' block n e x -> p (Graph' block' n' e x)
    go = \ case
        GNil -> pure GNil
        GUnit b -> GUnit <$> f b
        GMany e b x -> GMany <$> traverse f e <*> traverse f b <*> traverse f x

traverseGraph' :: Applicative p => NT (NT (Kleisli (->) p)) n n' -> NT (NT (Kleisli (->) p)) (Graph n) (Graph n')
traverseGraph' f = NT (NT (Kleisli (traverseGraphBlocks (traverseBlock (kleisli (nt (nt f)))))))


-- -----------------------------------------------------------------------------
-- Folds

-- | Fold a function over every node in a graph.
-- The fold function must be polymorphic in the shape of the nodes.

foldGraphNodes :: forall n a .
                  (forall e x . n e x       -> a -> a)
               -> (forall e x . Graph n e x -> a -> a)
foldGraphNodes f = foldGraphNodes3 (f, f, f)

foldGraphNodes3 :: forall n a .
                   (n C O -> a -> a, n O O -> a -> a, n O C -> a -> a)
                -> (forall e x . Graph n e x -> a -> a)
foldGraphNodes3 (fCO, fOO, fOC) = graph
    where graph :: forall e x . Graph n e x -> a -> a
          lift  :: forall thing ex . (thing -> a -> a) -> (MaybeO ex thing -> a -> a)

          graph GNil              = id
          graph (GUnit b)         = block b
          graph (GMany e b x)     = lift block e . flip (foldr block) b . lift block x
          lift _ NothingO         = id
          lift f (JustO thing)    = f thing

          block :: Block n e x -> IndexedCO e a a -> IndexedCO x a a
          block = foldBlockNodesF3 (fCO, fOO, fOC)


----------------------------------------------------------------

class LabelsPtr l where
  targetLabels :: l -> [Label]

instance NonLocal n => LabelsPtr (n e C) where
  targetLabels = successors

instance LabelsPtr Label where
  targetLabels = pure

instance LabelsPtr LabelSet where
  targetLabels = Set.toList

instance LabelsPtr l => LabelsPtr [l] where
  targetLabels = concatMap targetLabels


-- | Traversal: 'postorder_dfs' returns a list of blocks reachable
-- from the entry of enterable graph. The entry and exit are *not* included.
-- The list has the following property:
--
--	Say a "back reference" exists if one of a block's
--	control-flow successors precedes it in the output list
--
--	Then there are as few back references as possible
--
-- The output is suitable for use in
-- a forward dataflow problem.  For a backward problem, simply reverse
-- the list.  ('postorder_dfs' is sufficiently tricky to implement that
-- one doesn't want to try and maintain both forward and backward
-- versions.)

postorder_dfs :: NonLocal (block n) => Graph' block n O x -> [block n C C]
preorder_dfs  :: NonLocal (block n) => Graph' block n O x -> [block n C C]

-- | This is the most important traversal over this data structure.  It drops
-- unreachable code and puts blocks in an order that is good for solving forward
-- dataflow problems quickly.  The reverse order is good for solving backward
-- dataflow problems quickly.  The forward order is also reasonably good for
-- emitting instructions, except that it will not usually exploit Forrest
-- Baskett's trick of eliminating the unconditional branch from a loop.  For
-- that you would need a more serious analysis, probably based on dominators, to
-- identify loop headers.
--
-- The ubiquity of 'postorder_dfs' is one reason for the ubiquity of the 'LGraph'
-- representation, when for most purposes the plain 'Graph' representation is
-- more mathematically elegant (but results in more complicated code).
--
-- Here's an easy way to go wrong!  Consider
-- @
--	A -> [B,C]
--	B -> D
--	C -> D
-- @
-- Then ordinary dfs would give [A,B,D,C] which has a back ref from C to D.
-- Better to get [A,B,C,D]


graphDfs :: (LabelMap (block n C C) -> block n O C -> LabelSet -> [block n C C])
         -> (Graph' block n O x -> [block n C C])
graphDfs _     (GNil)    = []
graphDfs _     (GUnit{}) = []
graphDfs order (GMany (JustO entry) body _) = order body entry Set.empty

postorder_dfs = graphDfs postorder_dfs_from_except
preorder_dfs  = graphDfs preorder_dfs_from_except

postorder_dfs_from_except :: forall block e . (NonLocal block, LabelsPtr e)
                          => LabelMap (block C C) -> e -> LabelSet -> [block C C]
postorder_dfs_from_except blocks b = vchildren (get_children b) pure []
 where
   vnode :: block C C -> ([block C C] -> LabelSet -> a) -> [block C C] -> LabelSet -> a
   vnode block cont acc visited
     | Set.member id' visited = cont acc visited
     | otherwise =
            let cont' = cont . (:) block in
            vchildren (get_children block) cont' acc (Set.insert id' visited)
      where id' = entryLabel block
   vchildren :: forall a. [block C C] -> ([block C C] -> LabelSet -> a) -> [block C C] -> LabelSet -> a
   vchildren = flip $ foldr vnode
   get_children :: forall l. LabelsPtr l => l -> [block C C]
   get_children = foldr add_id [] . targetLabels
   add_id id' = maybe id (:) $ lookupFact id' blocks

postorder_dfs_from
    :: (NonLocal block, LabelsPtr b) => LabelMap (block C C) -> b -> [block C C]
postorder_dfs_from blocks b = postorder_dfs_from_except blocks b Set.empty


----------------------------------------------------------------

marked :: Label -> State LabelSet Bool
marked l = state $ \v -> (Set.member l v, v)

mark   :: Label -> State LabelSet ()
mark   l = state $ \v -> ((), Set.insert l v)

preorder_dfs_from_except :: forall block e . (NonLocal block, LabelsPtr e)
                         => LabelMap (block C C) -> e -> LabelSet -> [block C C]
preorder_dfs_from_except blocks b visited =
    evalState (children (get_children b)) visited []
  where children = foldr (liftA2 (.) . visit) (pure id)
        visit :: block C C -> State LabelSet (HL (block C C))
        visit b = do already <- marked (entryLabel b)
                     if already then return id
                      else do mark (entryLabel b)
                              Compiler.Hoopl.Graph.cons b <$> children (get_children b)
        get_children :: forall l. LabelsPtr l => l -> [block C C]
        get_children = foldr add_id [] . targetLabels
        add_id id' = maybe id (:) $ lookupFact id' blocks

type HL a = [a] -> [a] -- Hughes list (constant-time concatenation)
cons :: a -> HL a -> HL a
cons a as tail = a : as tail


-- -----------------------------------------------------------------------------
-- Extracting Labels from graphs

labelsDefined :: forall block n e x . NonLocal (block n) => Graph' block n e x
              -> LabelSet
labelsDefined GNil      = Set.empty
labelsDefined (GUnit{}) = Set.empty
labelsDefined (GMany _ body x) = foldrWithKey addEntry (exitLabel x) body
  where addEntry :: forall a. Label -> a -> LabelSet -> LabelSet
        addEntry label _ = Set.insert label
        exitLabel :: MaybeO x (block n C O) -> LabelSet
        exitLabel = foldr (Set.insert . entryLabel) Set.empty

labelsUsed :: forall block n e x. NonLocal (block n) => Graph' block n e x
           -> LabelSet
labelsUsed GNil      = Set.empty
labelsUsed (GUnit{}) = Set.empty
labelsUsed (GMany e body _) = foldr addTargets (entryTargets e) body
  where addTargets :: forall e. block n e C -> LabelSet -> LabelSet
        addTargets = flip (foldr Set.insert) . successors
        entryTargets :: MaybeO e (block n O C) -> LabelSet
        entryTargets = foldr addTargets Set.empty

externalEntryLabels :: forall n .
                       NonLocal n => LabelMap (Block n C C) -> LabelSet
externalEntryLabels body = defined `Set.difference` used
  where defined = labelsDefined g
        used = labelsUsed g
        g = GMany NothingO body NothingO

