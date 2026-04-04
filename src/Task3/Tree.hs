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

repairTermInsert :: (Measured m a) => a -> Tree m a -> TaggedTree m a
repairTermInsert x (Leaf ey) = Pseudo $ (node2 (leaf x) (leaf ey))
repairTermInsert _ _ = error "unreachable"

repairLeftInsert :: (Measured m a) => Tree m a -> TaggedTree m a -> TaggedTree m a
repairLeftInsert (Node2 _ _ r) tagged = case tagged of
  Pseudo (Node2 _ ll lr) -> Plain $ node3 ll lr r
  Plain l' -> Plain $ node2 l' r
  _ -> error "unreachable"
repairLeftInsert (Node3 _ _ m r) tagged = case tagged of
  Pseudo (Node2 _ ll lr) -> Pseudo $ node2 (node2 ll lr) (node2 m r)
  Plain l' -> Plain $ node3 l' m r
  _ -> error "unreachable"
repairLeftInsert _ _ = error "unreachable"

repairRightInsert :: (Measured m a) => Tree m a -> TaggedTree m a -> TaggedTree m a
repairRightInsert (Node2 _ l _) tagged = case tagged of
  Pseudo (Node2 _ rl rr) -> Plain $ node3 l rl rr
  Plain r' -> Plain $ node2 l r'
  _ -> error "unreachable"
repairRightInsert (Node3 _ l m _) tagged = case tagged of
  Pseudo (Node2 _ rl rr) -> Pseudo $ node2 (node2 l m) (node2 rl rr)
  Plain r' -> Plain $ node3 l m r'
  _ -> error "unreachable"
repairRightInsert _ _ = error "unreachable"

repairMidInsert :: (Measured m a) => Tree m a -> TaggedTree m a -> TaggedTree m a
repairMidInsert (Node3 _ l _ r) tagged = case tagged of
  Pseudo (Node2 _ ml mr) -> Pseudo $ node2 (node2 l ml) (node2 mr r)
  Plain m' -> Plain $ node3 l m' r
  _ -> error "unreachable"
repairMidInsert _ _ = error ""

repairLeftRemove :: (Measured m a) => Tree m a -> Tree m a -> TaggedTree m a -> TaggedTree m a
repairLeftRemove (Node2 _ _ r) sibling taggedLeft = case (sibling, taggedLeft) of
  (Node2 _ rl rr, Pseudo l') -> Pseudo $ node3 l' rl rr
  (Node3 _ rl rm rr, Pseudo l') -> Plain $ node2 (node2 l' rl) (node2 rm rr)
  (_, Plain l') -> Plain $ node2 l' r
  (_, _) -> error "unreachable"
repairLeftRemove (Node3 _ _ m r) sibling taggedLeft = case (sibling, taggedLeft) of
  (Node2 _ ml mr, Pseudo l') -> Plain $ node2 (node3 l' ml mr) r
  (Node3 _ ml mm mr, Pseudo l') -> Plain $ node3 (node2 l' ml) (node2 mm mr) r
  (_, Plain l') -> Plain $ node3 l' m r
  (_, _) -> error "unreachable"
repairLeftRemove _ _ _ = error "unreachable"

repairRightRemove :: (Measured m a) => Tree m a -> Tree m a -> TaggedTree m a -> TaggedTree m a
repairRightRemove (Node2 _ l _) sibling taggedRight = case (sibling, taggedRight) of
  (Node2 _ ll lr, Pseudo r') -> Pseudo $ node3 ll lr r'
  (Node3 _ ll lm lr, Pseudo r') -> Plain $ node2 (node2 ll lm) (node2 lr r')
  (_, Plain r') -> Plain $ node2 l r'
  (_, _) -> error "unreachable"
repairRightRemove (Node3 _ l m _) sibling taggedRight = case (sibling, taggedRight) of
  (Node2 _ ml mr, Pseudo r') -> Plain $ node2 l (node3 ml mr r')
  (Node3 _ ml mm mr, Pseudo r') -> Plain $ node3 l (node2 ml mm) (node2 mr r')
  (_, Plain r') -> Plain $ node3 l m r'
  (_, _) -> error "unreachable"
repairRightRemove _ _ _ = error "unreachable"

repairMidRemove :: (Measured m a) => Tree m a -> Tree m a -> TaggedTree m a -> TaggedTree m a
repairMidRemove (Node3 _ l _ r) sibling taggedMid = case (sibling, taggedMid) of
  (Node2 _ ll lr, Pseudo m') -> Plain $ node2 (node3 ll lr m') r
  (Node3 _ ll lm lr, Pseudo m') -> Plain $ node3 (node2 ll lm) (node2 lr m') r
  (_, Plain m') -> Plain $ node3 l m' r
  (_, _) -> error "unreachable"
repairMidRemove _ _ _ = error "unreachable"

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
