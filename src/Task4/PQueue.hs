{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task4.PQueue where

import Common.MonoidalTree
import Common.PriorityQueue
import Data.Foldable (Foldable (toList))
import Task1 (Max (..), Measured (..), Min (..), MinMax (..), Pick (pick))
import Task4.Tree

newtype PQueue k v = PQueue {getTree :: Tree (MinMax k) (Entry k v)}
  deriving (Show, Eq)

-- | Priority queue entry wrapper
newtype Entry k v = Entry {getEntry :: (k, v)}
  deriving (Show, Eq)

-- | Measures given entry using both minimum and maximum priority 'k'
instance (Ord k) => Measured (MinMax k) (Entry k v) where
  measure (Entry (k, _)) = MinMax (Min k, Max k)

-- * Utility functions

extractWith :: forall f k v. (Ord k, Monoid (f k), Eq (f k), Pick f) => PQueue k v -> Maybe (v, PQueue k v)
extractWith (PQueue pq) = case splitTree (\mm -> pick @f mm <> pick @f (measure pq) == pick @f mm) mempty pq of
  Just (Split l (Entry (_, v)) r) -> Just (v, PQueue $ l >< r)
  _ -> Nothing

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue Empty
  toPriorityQueue = foldr (uncurry insert) empty
  entries (PQueue t) = map getEntry (toList t)
  insert k v (PQueue t) = PQueue (t |> (Entry (k, v)))
  extractMin = extractWith @Min
  extractMax = extractWith @Max

-- extractMin = extractWith (\(MinMax (m, _)) -> m)
