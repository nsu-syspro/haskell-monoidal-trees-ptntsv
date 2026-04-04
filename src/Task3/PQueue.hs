{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3.PQueue where

import Common.MonoidalTree
import Common.PriorityQueue (PriorityQueue (..))
import Data.Foldable (Foldable (toList))
import Task1 (Max (..), Measured (..), Min (..), MinMax (..), Pick (pick))
import Task3.Tree

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
  let repairWith f maybeResult = fmap (\(v, result) -> (v, f result)) maybeResult
      go (Node2 _ (Leaf l@(Entry (_, v1))) (Leaf r@(Entry (_, v2))))
        | lm <> rm == lm = Just (v1, Pseudo $ leaf r)
        | otherwise = Just (v2, Pseudo $ leaf l)
        where
          lm = pick @f (measure l :: MinMax k)
          rm = pick @f (measure r :: MinMax k)
      go (Node3 _ (Leaf l@(Entry (_, v1))) (Leaf m@(Entry (_, v2))) (Leaf r@(Entry (_, v3))))
        | lm <> mm <> rm == lm = Just (v1, Plain $ node2 (leaf m) (leaf r))
        | lm <> mm <> rm == mm = Just (v2, Plain $ node2 (leaf l) (leaf r))
        | otherwise = Just (v3, Plain $ node2 (leaf l) (leaf m))
        where
          lm = pick @f (measure l :: MinMax k)
          mm = pick @f (measure m :: MinMax k)
          rm = pick @f (measure r :: MinMax k)
      go t'@(Node2 _ l r)
        | lm <> rm == lm = repairWith (repairLeftRemove t' r) (go l)
        | otherwise = repairWith (repairRightRemove t' l) (go r)
        where
          lm = pick @f (measure l :: MinMax k)
          rm = pick @f (measure r :: MinMax k)
      go t'@(Node3 _ l m r)
        | lm <> mm <> rm == lm = repairWith (repairLeftRemove t' m) (go l)
        | lm <> mm <> rm == mm = repairWith (repairMidRemove t' l) (go m)
        | otherwise = repairWith (repairRightRemove t' m) (go r)
        where
          lm = pick @f (measure l :: MinMax k)
          mm = pick @f (measure m :: MinMax k)
          rm = pick @f (measure r :: MinMax k)
      go (Leaf (Entry (_, v))) = Just (v, Plain $ Empty)
      go Empty = Nothing
   in case go t of
        Just (v, Pseudo t') -> Just (v, PQueue t')
        Just (v, Plain t') -> Just (v, PQueue t')
        _ -> Nothing

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue $ Empty
  toPriorityQueue = foldr (uncurry insert) empty

  entries (PQueue t) = map getEntry (toList t)

  insert :: forall k v. (Ord k) => k -> v -> PQueue k v -> PQueue k v
  insert k v (PQueue t) = PQueue (t |> (Entry (k, v)))

  extractMin = extractWith @Min
  extractMax = extractWith @Max
