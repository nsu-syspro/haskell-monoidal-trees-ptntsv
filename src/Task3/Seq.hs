{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3.Seq where

import Common.MonoidalTree (MonoidalTree (..))
import Common.Sequence
import Task1 (Measured (..), Size (..))
import Task3.Tree

-- * Sequence definition

-- | Random-access sequence based on binary tree
newtype Seq a = Seq {getTree :: Tree (Size a) (Elem a)}
  deriving (Show, Eq)

-- | Sequence element wrapper
newtype Elem a = Elem {getElem :: a}
  deriving (Show, Eq)

-- | Measures given element as 'Size 1'
instance Measured (Size a) (Elem a) where
  measure _ = Size 1

instance Foldable Seq where
  foldMap f (Seq t) = foldMap (f . getElem) t

  -- An O(1) implementation of length is possible
  -- due to size of the tree being cached at each node
  length :: forall a. Seq a -> Int
  length (Seq t) = getSize (measure t :: Size a)

-- * Sequence instance

instance Sequence Seq where
  empty = Seq Empty
  toSequence = foldr (+|) (Seq Empty)
  (+|) a (Seq t) = Seq (Elem a <| t)
  (|+) (Seq t) a = Seq (t |> Elem a)
  insertAt :: forall a. Int -> a -> Seq a -> Seq a
  insertAt i x s@(Seq t)
    | i <= 0 = Seq $ Elem x <| t
    | i >= length s = Seq $ t |> Elem x
    | otherwise =
        let go 0 (Leaf ey) = Pseudo $ node2 (leaf $ Elem x) (leaf ey)
            go n (Node2 _ l r)
              | n < lSize = case go n l of
                  Pseudo (Node2 _ ll lr) -> Plain $ node3 ll lr r
                  Plain l' -> Plain $ node2 l' r
                  _ -> error "unreachable"
              | otherwise = case go (n - lSize) r of
                  Pseudo (Node2 _ rl rr) -> Plain $ node3 l rl rr
                  Plain r' -> Plain $ node2 l r'
                  _ -> error "unreachable"
              where
                lSize = getSize (measure l :: Size a)
            go n (Node3 _ l m r)
              | n < lSize = case go n l of
                  Pseudo l'@(Node2 _ _ _) -> Pseudo $ node2 l' (node2 m r)
                  Plain l' -> Plain $ node3 l' m r
                  _ -> error "unreachable"
              | n < mSize = case go (n - lSize) m of
                  Pseudo (Node2 _ ml mr) -> Pseudo $ node2 (node2 l ml) (node2 mr r)
                  Plain m' -> Plain $ node3 l m' r
                  _ -> error "unreachable"
              | otherwise = case go (n - mSize) r of
                  Pseudo r'@(Node2 _ _ _) -> Pseudo $ node2 r' (node2 l m)
                  Plain r' -> Plain $ node3 l m r'
                  _ -> error "unreachable"
              where
                lSize = getSize (measure l :: Size a)
                mSize = lSize + getSize (measure m :: Size a)
            go _ t' = Plain $ t'
         in case go i t of
              Pseudo t' -> Seq t'
              Plain t' -> Seq t'

  removeAt = error "TODO: define removeAt (Sequence Task3.Seq)"

  elemAt :: forall a. Int -> Seq a -> Maybe a
  elemAt i (Seq t) = go i t
    where
      go 0 (Leaf x) = Just (getElem x)
      go n (Node2 _ l r)
        | n < lSize = go n l
        | otherwise = go (n - lSize) r
        where
          lSize = getSize (measure l :: Size a)
      go n (Node3 _ l m r)
        | n < lSize = go n l
        | n < mSize = go (n - lSize) m
        | otherwise = go (n - mSize) r
        where
          lSize = getSize (measure l :: Size a)
          mSize = lSize + getSize (measure m :: Size a)
      go _ _ = Nothing
