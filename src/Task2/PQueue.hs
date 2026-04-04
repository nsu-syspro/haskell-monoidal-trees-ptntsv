{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2.PQueue where

import Common.MonoidalTree (MonoidalTree (..))
import Common.PriorityQueue
import Task1 (Max (..), Measured (..), Min (..), MinMax (..), Pick (..))
import Task2.Tree

-- * Priority queue definition

-- | Priority queue based on binary tree
newtype PQueue k v = PQueue {getTree :: Tree (MinMax k) (Entry k v)}
  deriving (Show, Eq)

-- | Priority queue entry wrapper
newtype Entry k v = Entry {getEntry :: (k, v)}
  deriving (Show, Eq)

instance (Ord k) => Measured (MinMax k) (Entry k v) where
  measure (Entry (k, _)) = MinMax (Min k, Max k)

-- * Utility functions

extractWith :: forall f k v. (Ord k, Monoid (f k), Eq (f k), Pick f) => PQueue k v -> Maybe (v, PQueue k v)
extractWith (PQueue t) =
  let go (Branch _ l r)
        | lmm <> rmm == lmm = case go l of
            Just (v, Empty) -> Just (v, r)
            Just (v, lWithoutMin) -> Just (v, branch lWithoutMin r)
            _ -> Nothing
        | otherwise = case go r of
            Just (v, Empty) -> Just (v, l)
            Just (v, rWithoutMin) -> Just (v, branch l rWithoutMin)
            _ -> Nothing
        where
          lmm = pick @f (measure l :: MinMax k)
          rmm = pick @f (measure r :: MinMax k)
      go (Leaf (Entry (_, v))) = Just (v, Empty)
      go _ = Nothing
   in case go t of
        Just (v, t') -> Just (v, PQueue t')
        _ -> Nothing

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue Empty
  toPriorityQueue = foldr (uncurry insert) empty
  entries (PQueue t) = go [] t
    where
      go acc Empty = acc
      go acc (Leaf a) = getEntry a : acc
      go acc (Branch _ l r) = go (go acc l) r

  insert k v (PQueue t) = PQueue $ (Entry (k, v)) <| t

  extractMin :: forall k v. (Ord k) => PQueue k v -> Maybe (v, PQueue k v)
  extractMin = extractWith @Min

  extractMax :: forall k v. (Ord k) => PQueue k v -> Maybe (v, PQueue k v)
  extractMax = extractWith @Max
