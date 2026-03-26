{-# LANGUAGE ScopedTypeVariables #-}
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

data TaggedTree m a = Plain (Tree m a) | Pseudo (Tree m a)

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
  toTree = foldr (<|) Empty
  (<|) x t =
    let prepend Empty = Plain $ leaf x
        prepend (Leaf y) = Pseudo $ node2 (leaf x) (leaf y)
        prepend (Node2 _ l r) = case prepend l of
          Pseudo (Node2 _ ll lr) -> Plain $ node3 ll lr r
          Plain n -> Plain $ node2 n r
          _ -> error "unreachable"
        prepend (Node3 _ l m r) = case prepend l of
          Pseudo pn@(Node2 _ _ _) -> Pseudo $ node2 pn (node2 m r)
          Plain n -> Plain $ node3 n m r
          _ -> error "unreachable"
     in case prepend t of
          Pseudo t' -> t'
          Plain t' -> t'
  (|>) t x =
    let append Empty = Plain $ leaf x
        append (Leaf y) = Pseudo $ node2 (leaf y) (leaf x)
        append (Node2 _ l r) = case append r of
          Pseudo (Node2 _ rl rr) -> Plain $ node3 l rl rr
          Plain n -> Plain $ node2 l n
          _ -> error "unreachable"
        append (Node3 _ l m r) = case append r of
          Pseudo pn@(Node2 _ _ _) -> Pseudo $ node2 (node2 l m) pn
          Plain n -> Plain $ node3 l m n
          _ -> error "unreachable"
     in case append t of
          Pseudo t' -> t'
          Plain t' -> t'
