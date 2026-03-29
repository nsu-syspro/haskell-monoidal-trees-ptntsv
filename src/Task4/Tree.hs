{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task4.Tree where

import Common.MonoidalTree
import Task1 (Measured (..))

-- * Finger tree definition

-- | Finger tree with values 'a' in leaves
-- Intermediate branches contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Single a
  | Deep m (Digit a) (Tree m (Node m a)) (Digit a)
  deriving (Show, Eq)

-- | 2-3 node of finger tree
data Node m a
  = Node2 m a a
  | Node3 m a a a
  deriving (Show, Eq)

-- | Finger tree digit
data Digit a
  = One a
  | Two a a
  | Three a a a
  | Four a a a a
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance (Measured m a) => Measured m (Tree m a) where
  measure Empty = mempty
  measure (Single x) = measure x
  measure (Deep m _ _ _) = m

-- | Measures given node using provided measure of 'a'
instance (Measured m a) => Measured m (Node m a) where
  measure (Node2 m _ _) = m
  measure (Node3 m _ _ _) = m

-- | Measures given digit using provided measure of 'a'
instance {-# OVERLAPPING #-} (Measured m a) => Measured m (Digit a) where
  measure = foldMap measure

instance Foldable (Tree m) where
  foldMap _ Empty = mempty
  foldMap f (Single x) = f x
  foldMap f (Deep _ d1 t d2) = foldMap f d1 <> foldMap (foldMap f) t <> foldMap f d2

instance Foldable (Node m) where
  foldMap f (Node2 _ l r) = f l <> f r
  foldMap f (Node3 _ l m r) = f l <> f m <> f r

instance Foldable Digit where
  foldMap f (One x1) = f x1
  foldMap f (Two x1 x2) = f x1 <> f x2
  foldMap f (Three x1 x2 x3) = f x1 <> f x2 <> f x3
  foldMap f (Four x1 x2 x3 x4) = f x1 <> f x2 <> f x3 <> f x4

-- * Smart constructors

single :: a -> Tree m a
single = Single

node2 :: (Measured m a) => a -> a -> Node m a
node2 l r = Node2 (measure l <> measure r) l r

node3 :: (Measured m a) => a -> a -> a -> Node m a
node3 l m r = Node3 (measure l <> measure m <> measure r) l m r

deep :: forall m a. (Measured m a) => Digit a -> Tree m (Node m a) -> Digit a -> Tree m a
deep d1 m d2 = Deep (measure d1 <> measure m <> measure d2) d1 m d2

-- * Monoidal tree instance

instance {-# OVERLAPPING #-} MonoidalTree Tree where
  toTree = error "TODO: define toTree (MonoidalTree Task4.Tree)"

  -- (<|) :: (Measured m a, Measured m (Digit a)) => a -> Tree m a -> Tree m a
  (<|) x Empty = Single x
  (<|) x (Single x1) = deep (One x) Empty (One x1)
  (|>) = error "TODO: define (|>) (MonoidalTree Task4.Tree)"

-- * Utility functions

-- | Split result with left part, middle element and right part
data Split f a = Split (f a) a (f a)
  deriving (Show, Eq)

-- | Helper function for spliting tree based on given predicate and starting accumulator value
splitTree :: (Measured m a) => (m -> Bool) -> m -> Tree m a -> Maybe (Split (Tree m) a)
splitTree = error "TODO: define splitTree"

-- | Splits tree based on given predicate
split :: (Measured m a) => (m -> Bool) -> Tree m a -> (Tree m a, Tree m a)
split = error "TODO: define split"

-- | Concatenates two trees
infixr 6 ><

(><) :: (Measured m a) => Tree m a -> Tree m a -> Tree m a
(><) = error "TODO: define (><)"
