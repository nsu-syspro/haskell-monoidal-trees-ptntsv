{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3.Tree where

import Common.MonoidalTree
import Task1 (Measured (..))

-- * 2-3 tree definition

-- | 2-3 tree with values 'a' in leaves
-- Intermediate nodes contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Leaf a
  | Node2 m (Tree m a) (Tree m a)
  | Node3 m (Tree m a) (Tree m a) (Tree m a)
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance (Measured m a) => Measured m (Tree m a) where
  measure Empty = mempty
  measure (Leaf a) = measure a
  measure (Node2 m _ _) = m
  measure (Node3 m _ _ _) = m

instance Foldable (Tree m) where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node2 _ l r) = foldMap f l <> foldMap f r
  foldMap f (Node3 _ l m r) = foldMap f l <> foldMap f m <> foldMap f r

-- * Smart constructors

leaf :: a -> Tree m a
leaf = Leaf

node2 :: (Measured m a) => Tree m a -> Tree m a -> Tree m a
node2 l r = Node2 (measure l <> measure r) l r

node3 :: (Measured m a) => Tree m a -> Tree m a -> Tree m a -> Tree m a
node3 l m r = Node3 (measure l <> measure m <> measure r) l m r

-- * Monoidal tree instance

instance MonoidalTree Tree where
  toTree = error "TODO: define toTree (MonoidalTree Task3.Tree)"
  (<|) = error "TODO: define (<|) (MonoidalTree Task3.Tree)"
  (|>) = error ""

-- (|>) t@(Leaf x) y
--   | measure t < measure y = node2 (leaf x) (leaf y)
--   | otherwise = node2 (leaf y) (leaf x)
