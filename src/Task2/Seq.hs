{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2.Seq where

import Common.MonoidalTree (MonoidalTree (..))
import Common.Sequence
import Task1 (Measured (..), Size (..))
import Task2.Tree

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
  toSequence = foldr (+|) empty

  (+|) a (Seq t) = Seq (Elem a <| t)
  (|+) (Seq t) a = Seq (t |> Elem a)

  insertAt :: forall a. Int -> a -> Seq a -> Seq a
  insertAt i a s@(Seq t)
    | i <= 0 = a +| s
    | i >= length s = s |+ a
    | otherwise = Seq (go i t)
    where
      go 0 (Leaf x) = branch (leaf (Elem a)) (leaf x)
      go n (Branch _ l r)
        | n < lSize = branch (go n l) r
        | otherwise = branch l (go (n - lSize) r)
        where
          lSize = getSize (measure l :: Size a)
      go _ t' = t'

  removeAt :: forall a. Int -> Seq a -> Seq a
  removeAt i s@(Seq t)
    | i < 0 || i >= (length s) = s
    | otherwise = Seq (go i t)
    where
      go n (Branch _ l@(Leaf _) r@(Leaf _))
        | n == 0 = r
        | n == 1 = l
      go n (Branch _ l@(Leaf _) r@(Branch _ _ _))
        | n == 0 = r
        | otherwise = branch l (go (n - 1) r)
      go n (Branch _ l@(Branch (Size lSize) _ _) r@(Branch _ _ _))
        | n < lSize = branch (go n l) r
        | otherwise = branch l (go (n - lSize) r)
      go n (Branch _ l@(Branch (Size lSize) _ _) r@(Leaf _))
        | n < lSize = branch (go n l) r
        | otherwise = l
      go _ _ = Empty

  elemAt :: forall a. Int -> Seq a -> Maybe a
  elemAt i (Seq t) = go i t
    where
      go 0 (Leaf x) = Just (getElem x)
      go n (Branch _ l r)
        | n < lSize = go n l
        | otherwise = go (n - lSize) r
        where
          lSize = getSize (measure l :: Size a)
      go _ _ = Nothing
