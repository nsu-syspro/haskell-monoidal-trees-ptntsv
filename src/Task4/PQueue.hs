{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task4.PQueue where

import Common.MonoidalTree
import Common.PriorityQueue
import Data.Foldable (Foldable (toList))
import Task1 (Max (..), Measured (..), Min (..), MinMax (..))
import Task4.Tree

newtype PQueue k v = PQueue {getTree :: Tree (MinMax k) (Entry k v)}
  deriving (Show, Eq)

-- | Priority queue entry wrapper
newtype Entry k v = Entry {getEntry :: (k, v)}
  deriving (Show, Eq)

-- | Measures given entry using both minimum and maximum priority 'k'
instance (Ord k) => Measured (MinMax k) (Entry k v) where
  measure (Entry (k, _)) = MinMax (Min k, Max k)

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue Empty
  toPriorityQueue = foldr (uncurry insert) empty
  entries (PQueue t) = map getEntry (toList t)
  insert k v (PQueue t) = PQueue (t |> (Entry (k, v)))
  extractWith pick (PQueue pq) = case splitTree (\mm -> pick mm <> pick (measure pq) == pick mm) mempty pq of
    Just (Split l (Entry (_, v)) r) -> Just (v, PQueue $ l >< r)
    _ -> Nothing
  extractMax = extractWith (\(MinMax (_, m)) -> m)
  extractMin = extractWith (\(MinMax (m, _)) -> m)
