{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task4.Seq where

import Common.Sequence
import Task1 (Measured (..), Size (..))
import Task4.Tree

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
  foldMap = error "TODO: define foldMap (Foldable Task4.Seq)"

  -- An O(1) implementation of length is possible
  -- due to size of the tree being cached at each node
  length :: forall a. Seq a -> Int
  length (Seq t) = getSize $ (measure t :: Size a)

-- * Sequence instance

instance Sequence Seq where
  empty = Seq $ Empty
  toSequence = foldr (+|) empty
  (+|) = error "TODO: define (+|) (Sequence Task2.Seq)"
  (|+) = error "TODO: define (|+) (Sequence Task2.Seq)"
  insertAt = error "TODO: define insertAt (Sequence Task2.Seq)"
  removeAt = error "TODO: define removeAt (Sequence Task2.Seq)"
  elemAt = error "TODO: define elemAt (Sequence Task2.Seq)"
