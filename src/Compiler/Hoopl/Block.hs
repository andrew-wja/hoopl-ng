{-# LANGUAGE CPP, GADTs, TypeFamilies, ScopedTypeVariables, RankNTypes,
             MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Block (
    -- * Shapes
    O, C
  , MaybeO(..), MaybeC(..)
  , IndexedCO
  , Shape(..)
  , shape'

    -- * Blocks
  , Block(..)

    -- ** Predicates on Blocks
  , isEmpty
  , shape

    -- ** Constructing blocks
  , empty, cons, snoc
  , joinHead, joinTail, join, joinAny
  , append

    -- ** Deconstructing blocks
  , firstNode, lastNode, endNodes
  , splitHead, splitTail, split, splitAny

    -- ** Modifying blocks
  , replaceFirstNode, replaceLastNode

    -- ** Converting to and from lists
  , toList, fromList

    -- ** Maps and folds
  , mapBlock, mapBlock3, mapBlock', mapBlock3'
  , foldBlockNodesF, foldBlockNodesF3
  , foldBlockNodesB, foldBlockNodesB3
  , traverseBlock, traverseBlock3

    -- ** Biasing
  , frontBias, backBias

  ) where

import Control.Categorical.Functor (NT (..))
import qualified Control.Categorical.Functor as C
import Control.Categorical.Monad (Kleisli (..))
import qualified Control.Categorical.Monad as C
import Control.Monad.StrictIdentity
import Data.Functor.Identity (Identity (..))

-- -----------------------------------------------------------------------------
-- Shapes: Open and Closed

-- | Used at the type level to indicate an "open" structure with
-- a unique, unnamed control-flow edge flowing in or out.         
-- "Fallthrough" and concatenation are permitted at an open point.
data O 
       
-- | Used at the type level to indicate a "closed" structure which
-- supports control transfer only through the use of named
-- labels---no "fallthrough" is permitted.  The number of control-flow
-- edges is unconstrained.
data C

-- | Either type indexed by closed/open using type families
type family IndexedCO ex a b where
    IndexedCO C a _b = a
    IndexedCO O _a b = b

-- | Maybe type indexed by open/closed
data MaybeO ex t where
  JustO    :: t -> MaybeO O t
  NothingO ::      MaybeO C t

deriving instance Foldable (MaybeO ex)
deriving instance Functor (MaybeO ex)
deriving instance Traversable (MaybeO ex)

-- | Maybe type indexed by closed/open
data MaybeC ex t where
  JustC    :: t -> MaybeC C t
  NothingC ::      MaybeC O t

deriving instance Foldable (MaybeC ex)
deriving instance Functor (MaybeC ex)
deriving instance Traversable (MaybeC ex)


-- | Dynamic shape value
data Shape ex where
  Closed :: Shape C
  Open   :: Shape O

shape' :: a -> b -> Shape x -> IndexedCO x a b
shape' = \ a b -> \ case
    Closed -> a
    Open -> b


-- -----------------------------------------------------------------------------
-- The Block type

-- | A sequence of nodes.  May be any of four shapes (O/O, O/C, C/O, C/C).
-- Open at the entry means single entry, mutatis mutandis for exit.
-- A closed/closed block is a /basic/ block and can't be extended further.
-- Clients should avoid manipulating blocks and should stick to either nodes
-- or graphs.
data Block n e x where
  BlockCO  :: n C O -> Block n O O          -> Block n C O
  BlockCC  :: n C O -> Block n O O -> n O C -> Block n C C
  BlockOC  ::          Block n O O -> n O C -> Block n O C

  BNil    :: Block n O O
  BMiddle :: n O O                      -> Block n O O
  BCat    :: Block n O O -> Block n O O -> Block n O O
  BSnoc   :: Block n O O -> n O O       -> Block n O O
  BCons   :: n O O       -> Block n O O -> Block n O O

instance (Applicative m, C.Monad (->) m) => C.Functor (NT (NT (Kleisli (->) m))) (NT (NT (Kleisli (->) m))) Block where
    map f' = NT (NT (Kleisli (traverseBlock (kleisli (nt (nt f'))))))

instance C.Functor (NT (NT (->))) (NT (NT (->))) Block where
    map f' = NT (NT (mapBlock (nt (nt f'))))

-- -----------------------------------------------------------------------------
-- Simple operations on Blocks

-- Predicates

isEmpty :: Block n e x -> Bool
isEmpty BNil       = True
isEmpty (BCat l r) = isEmpty l && isEmpty r
isEmpty _          = False

shape :: Block n e x -> (Shape e, Shape x)
shape = \ case
    BlockCO _ _ -> (Closed, Open)
    BlockCC _ _ _ -> (Closed, Closed)
    BlockOC _ _ -> (Open, Closed)
    BNil -> (Open, Open)
    BMiddle _ -> (Open, Open)
    BCat _ _ -> (Open, Open)
    BSnoc _ _ -> (Open, Open)
    BCons _ _ -> (Open, Open)


-- Building

empty :: Block n O O
empty = BNil

cons :: n O O -> Block n O x -> Block n O x
cons n b = case b of
  BlockOC b l  -> (BlockOC $! (n `cons` b)) l
  BNil{}    -> BMiddle n
  BMiddle{} -> n `BCons` b
  BCat{}    -> n `BCons` b
  BSnoc{}   -> n `BCons` b
  BCons{}   -> n `BCons` b

snoc :: Block n e O -> n O O -> Block n e O
snoc b n = case b of
  BlockCO f b -> BlockCO f $! (b `snoc` n)
  BNil{}      -> BMiddle n
  BMiddle{}   -> b `BSnoc` n
  BCat{}      -> b `BSnoc` n
  BSnoc{}     -> b `BSnoc` n
  BCons{}     -> b `BSnoc` n

joinHead :: n C O -> Block n O x -> Block n C x
joinHead f (BlockOC b l) = BlockCC f b l
joinHead f b = BlockCO f BNil `cat` b

joinTail :: Block n e O -> n O C -> Block n e C
joinTail (BlockCO f b) t = BlockCC f b t
joinTail b t = b `cat` BlockOC BNil t

join :: n C O -> Block n O O -> n O C -> Block n C C
join f b t = BlockCC f b t

append :: Block n e O -> Block n O x -> Block n e x
append = cat


-- Taking apart

firstNode :: Block n C x -> n C O
firstNode (BlockCO n _)   = n
firstNode (BlockCC n _ _) = n

lastNode :: Block n x C -> n O C
lastNode (BlockOC   _ n) = n
lastNode (BlockCC _ _ n) = n

endNodes :: Block n C C -> (n C O, n O C)
endNodes (BlockCC f _ l) = (f,l)

splitHead :: Block n C x -> (n C O, Block n O x)
splitHead (BlockCO n b)   = (n, b)
splitHead (BlockCC n b t) = (n, BlockOC b t)

splitTail :: Block n e C -> (Block n e O, n O C)
splitTail (BlockOC b n)   = (b, n)
splitTail (BlockCC f b t) = (BlockCO f b, t)

-- | Split a closed block into its entry node, open middle block, and
-- exit node.
split :: Block n C C -> (n C O, Block n O O, n O C)
split (BlockCC f b t) = (f, b, t)

splitAny :: Block n e x
              -> (MaybeC e (n C O), Block n O O, MaybeC x (n O C))
splitAny block = case block of
  BlockCO f b   -> (JustC f,  b, NothingC)
  BlockCC f b l -> (JustC f,  b, JustC l)
  BlockOC   b l -> (NothingC, b, JustC l)
  b@BNil        -> (NothingC, b, NothingC)
  b@BMiddle{}   -> (NothingC, b, NothingC)
  b@BCat{}      -> (NothingC, b, NothingC)
  b@BCons{}     -> (NothingC, b, NothingC)
  b@BSnoc{}     -> (NothingC, b, NothingC)

toList :: Block n O O -> [n O O]
toList b = go b []
   where go :: Block n O O -> [n O O] -> [n O O]
         go BNil         r = r
         go (BMiddle n)  r = n : r
         go (BCat b1 b2) r = go b1 $! go b2 r
         go (BSnoc b1 n) r = go b1 (n:r)
         go (BCons n b1) r = n : go b1 r

fromList :: [n O O] -> Block n O O
fromList = foldr BCons BNil


-- | Convert a list of nodes to a block. The entry and exit node must
-- or must not be present depending on the shape of the block.
--
joinAny :: (MaybeC e (n C O), Block n O O, MaybeC x (n O C)) -> Block n e x
joinAny (NothingC, m, NothingC)  = m
joinAny (NothingC, m, JustC l)   = BlockOC   m l
joinAny (JustC f, m, NothingC)   = BlockCO f m
joinAny (JustC f, m, JustC l)    = BlockCC f m l


-- Modifying

replaceFirstNode :: Block n C x -> n C O -> Block n C x
replaceFirstNode (BlockCO _ b)   f = BlockCO f b
replaceFirstNode (BlockCC _ b n) f = BlockCC f b n

replaceLastNode :: Block n x C -> n O C -> Block n x C
replaceLastNode (BlockOC   b _) n = BlockOC b n
replaceLastNode (BlockCC l b _) n = BlockCC l b n


-- -----------------------------------------------------------------------------
-- General concatenation

cat :: Block n e O -> Block n O x -> Block n e x
cat x y = case x of
  BNil -> y

  BlockCO l b1 -> case y of
                   BlockOC b2 n -> (BlockCC l $! (b1 `cat` b2)) n
                   BNil         -> x
                   BMiddle _    -> BlockCO l $! (b1 `cat` y)
                   BCat{}       -> BlockCO l $! (b1 `cat` y)
                   BSnoc{}      -> BlockCO l $! (b1 `cat` y)
                   BCons{}      -> BlockCO l $! (b1 `cat` y)

  BMiddle n -> case y of
                   BlockOC b2 n2 -> (BlockOC $! (x `cat` b2)) n2
                   BNil          -> x
                   BMiddle{}     -> BCons n y
                   BCat{}        -> BCons n y
                   BSnoc{}       -> BCons n y
                   BCons{}       -> BCons n y

  BCat{} -> case y of
                   BlockOC b3 n2 -> (BlockOC $! (x `cat` b3)) n2
                   BNil          -> x
                   BMiddle n     -> BSnoc x n
                   BCat{}        -> BCat x y
                   BSnoc{}       -> BCat x y
                   BCons{}       -> BCat x y

  BSnoc{} -> case y of
                   BlockOC b2 n2 -> (BlockOC $! (x `cat` b2)) n2
                   BNil          -> x
                   BMiddle n     -> BSnoc x n
                   BCat{}        -> BCat x y
                   BSnoc{}       -> BCat x y
                   BCons{}       -> BCat x y


  BCons{} -> case y of
                   BlockOC b2 n2 -> (BlockOC $! (x `cat` b2)) n2
                   BNil          -> x
                   BMiddle n     -> BSnoc x n
                   BCat{}        -> BCat x y
                   BSnoc{}       -> BCat x y
                   BCons{}       -> BCat x y


-- -----------------------------------------------------------------------------
-- Mapping

-- | map a function over the nodes of a 'Block'
mapBlock :: (forall e x. n e x -> n' e x) -> Block n e x -> Block n' e x
mapBlock f = runIdentity . traverseBlock (Identity . f)

-- | map over a block, with different functions to apply to first nodes,
-- middle nodes and last nodes respectively.  The map is non-strict.
mapBlock3 :: forall n n' e x .
            ( n C O -> n' C O
            , n O O -> n' O O,
              n O C -> n' O C)
         -> Block n e x -> Block n' e x
mapBlock3 (fCO, fOO, fOC) =
    runIdentity . traverseBlock3 (Identity . fCO, Identity . fOO, Identity . fOC)

-- | A strict 'mapBlock'
mapBlock' :: (forall e x. n e x -> n' e x) -> (Block n e x -> Block n' e x)
mapBlock' f = mapBlock3' (f, f, f)

-- | map over a block, with different functions to apply to first nodes,
-- middle nodes and last nodes respectively.  The map is strict.
mapBlock3' :: forall n n' e x .
             ( n C O -> n' C O
             , n O O -> n' O O,
               n O C -> n' O C)
          -> Block n e x -> Block n' e x
mapBlock3' (fCO, fOO, fOC) =
    runStrictIdentity . traverseBlock3 (StrictIdentity . fCO, StrictIdentity . fOO, StrictIdentity . fOC)

traverseBlock :: Applicative p => (∀ e x . n e x -> p (n' e x)) -> Block n e x -> p (Block n' e x)
traverseBlock f = traverseBlock3 (f, f, f)

traverseBlock3
 :: Applicative p
 => ( n C O -> p (n' C O)
    , n O O -> p (n' O O)
    , n O C -> p (n' O C)
    )
 -> Block n e x -> p (Block n' e x)
traverseBlock3 f@(fCO, fOO, fOC) = \ case
    BlockCO n b   -> BlockCO <$> fCO n <*> traverseBlock3 f b
    BlockOC   b n -> BlockOC           <$> traverseBlock3 f b <*> fOC n
    BlockCC n b m -> BlockCC <$> fCO n <*> traverseBlock3 f b <*> fOC m
    BNil          -> pure BNil
    BMiddle n     -> BMiddle <$> fOO n
    BCat b₁ b₂    -> BCat <$> traverseBlock3 f b₁ <*> traverseBlock3 f b₂
    BSnoc b n     -> BSnoc <$> traverseBlock3 f b <*> fOO n
    BCons n b     -> BCons <$> fOO n <*> traverseBlock3 f b

-- -----------------------------------------------------------------------------
-- Folding


-- | Fold a function over every node in a block, forward or backward.
-- The fold function must be polymorphic in the shape of the nodes.
foldBlockNodesF3 :: forall n a b c .
                   ( n C O       -> a -> b
                   , n O O       -> b -> b
                   , n O C       -> b -> c)
                 -> (forall e x . Block n e x -> IndexedCO e a b -> IndexedCO x c b)
foldBlockNodesF  :: forall n a .
                    (forall e x . n e x       -> a -> a)
                 -> (forall e x . Block n e x -> IndexedCO e a a -> IndexedCO x a a)
foldBlockNodesB3 :: forall n a b c .
                   ( n C O       -> b -> c
                   , n O O       -> b -> b
                   , n O C       -> a -> b)
                 -> (forall e x . Block n e x -> IndexedCO x a b -> IndexedCO e c b)
foldBlockNodesB  :: forall n a .
                    (forall e x . n e x       -> a -> a)
                 -> (forall e x . Block n e x -> IndexedCO x a a -> IndexedCO e a a)

foldBlockNodesF3 (ff, fm, fl) = block
  where block :: forall e x . Block n e x -> IndexedCO e a b -> IndexedCO x c b
        block (BlockCO f b  )   = ff f `cat` block b
        block (BlockCC f b l)   = ff f `cat` block b `cat` fl l
        block (BlockOC   b l)   =            block b `cat` fl l
        block BNil              = id
        block (BMiddle node)    = fm node
        block (b1 `BCat`    b2) = block b1 `cat` block b2
        block (b1 `BSnoc` n)    = block b1 `cat` fm n
        block (n `BCons` b2)    = fm n `cat` block b2
        cat :: forall a b c. (a -> b) -> (b -> c) -> a -> c
        cat f f' = f' . f

foldBlockNodesF f = foldBlockNodesF3 (f, f, f)

foldBlockNodesB3 (ff, fm, fl) = block
  where block :: forall e x . Block n e x -> IndexedCO x a b -> IndexedCO e c b
        block (BlockCO f b  )   = ff f `cat` block b
        block (BlockCC f b l)   = ff f `cat` block b `cat` fl l
        block (BlockOC   b l)   =            block b `cat` fl l
        block BNil              = id
        block (BMiddle node)    = fm node
        block (b1 `BCat`    b2) = block b1 `cat` block b2
        block (b1 `BSnoc` n)    = block b1 `cat` fm n
        block (n `BCons` b2)    = fm n `cat` block b2
        cat :: forall a b c. (b -> c) -> (a -> b) -> a -> c
        cat f f' = f . f'

foldBlockNodesB f = foldBlockNodesB3 (f, f, f)


----------------------------------------------------------------

-- | A block is "front biased" if the left child of every
-- concatenation operation is a node, not a general block; a
-- front-biased block is analogous to an ordinary list.  If a block is
-- front-biased, then its nodes can be traversed from front to back
-- without general recusion; tail recursion suffices.  Not all shapes
-- can be front-biased; a closed/open block is inherently back-biased.

frontBias :: Block n e x -> Block n e x
frontBias blk = case blk of
   BlockCO f b   -> BlockCO f (fb b BNil)
   BlockOC   b n -> BlockOC   (fb b BNil) n
   BlockCC f b n -> BlockCC f (fb b BNil) n
   b@BNil{}      -> fb b BNil
   b@BMiddle{}   -> fb b BNil
   b@BCat{}      -> fb b BNil
   b@BSnoc{}     -> fb b BNil
   b@BCons{}     -> fb b BNil
 where
   fb :: Block n O O -> Block n O O -> Block n O O
   fb BNil        rest = rest
   fb (BMiddle n) rest = BCons n rest
   fb (BCat l r)  rest = fb l (fb r rest)
   fb (BCons n b) rest = BCons n (fb b rest)
   fb (BSnoc b n) rest = fb b (BCons n rest)

-- | A block is "back biased" if the right child of every
-- concatenation operation is a node, not a general block; a
-- back-biased block is analogous to a snoc-list.  If a block is
-- back-biased, then its nodes can be traversed from back to back
-- without general recusion; tail recursion suffices.  Not all shapes
-- can be back-biased; an open/closed block is inherently front-biased.

backBias :: Block n e x -> Block n e x
backBias blk = case blk of
   BlockCO f b   -> BlockCO f (bb BNil b)
   BlockOC   b n -> BlockOC   (bb BNil b) n
   BlockCC f b n -> BlockCC f (bb BNil b) n
   b@BNil{}      -> bb BNil b
   b@BMiddle{}   -> bb BNil b
   b@BCat{}      -> bb BNil b
   b@BSnoc{}     -> bb BNil b
   b@BCons{}     -> bb BNil b
 where
   bb :: Block n O O -> Block n O O -> Block n O O
   bb rest BNil = rest
   bb rest (BMiddle n) = BSnoc rest n
   bb rest (BCat l r) = bb (bb rest l) r
   bb rest (BCons n b) = bb (BSnoc rest n) b
   bb rest (BSnoc b n) = BSnoc (bb rest b) n
