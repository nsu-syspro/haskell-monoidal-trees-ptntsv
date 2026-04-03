{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task4.Seq where

import Common.MonoidalTree
import Common.Sequence
import Task1 (Measured (..), Size (..))
import Task4.Tree
import Prelude hiding (splitAt)

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
  length (Seq t) = getSize $ (measure t :: Size a)

-- * Utility functions

after :: Int -> Size a -> Bool
after at = (> at) . getSize

splitAt :: (Measured (Size Int) a) => Int -> Tree (Size Int) a -> (Tree (Size Int) a, Tree (Size Int) a)
splitAt = split . after

-- * Sequence instance

instance Sequence Seq where
  empty = Seq $ Empty
  toSequence = foldr (+|) empty
  (+|) x (Seq t) = Seq ((Elem x) <| t)
  (|+) (Seq t) x = Seq (t |> (Elem x))

  insertAt at x (Seq t) = let (l, r) = split (after at) t in Seq ((l |> Elem x) >< r)
  removeAt at (Seq t) = Seq $ maybe t (\(Split l _ r) -> l >< r) (splitTree (after at) (Size 0) t) -- sic!

  elemAt _ (Seq Empty) = Nothing
  elemAt 0 (Seq (Single (Elem x))) = Just x
  elemAt at (Seq t) = (\(Split _ (Elem x) _) -> x) <$> splitTree (after at) (Size 0) t
