{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3.PQueue where

import Common.PriorityQueue
import Task1 (Max (..), Measured (..), Min (..), MinMax (..))
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

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue $ Empty
  toPriorityQueue = foldr (uncurry insert) empty
  entries (PQueue t) =
    let go acc Empty = acc
        go acc (Leaf kv) = getEntry kv : acc
        go acc (Node2 _ l r) = go (go acc l) r
        go acc (Node3 _ l m r) = go (go (go acc l) m) r
     in go [] t

  insert :: forall k v. (Ord k) => k -> v -> PQueue k v -> PQueue k v
  insert k v (PQueue t) =
    let go Empty = Plain $ leaf $ Entry (k, v)
        go (Leaf ey) = Pseudo $ node2 (leaf $ (Entry (k, v))) (leaf ey)
        go t'@(Node2 _ l r)
          | lk <> rk == lk = repairLeftInsert t' (go l)
          | otherwise = repairRightInsert t' (go r)
          where
            lk = measure l :: MinMax k
            rk = measure r :: MinMax k
        go t'@(Node3 _ l m r)
          | lk <> rk == lk = repairLeftInsert t' (go l)
          | lk <> mk == mk = repairMidInsert t' (go m)
          | otherwise = repairRightInsert t' (go r)
          where
            lk = measure l :: MinMax k
            mk = measure m :: MinMax k
            rk = measure r :: MinMax k
     in case go t of
          Pseudo t' -> PQueue t'
          Plain t' -> PQueue t'

  extractWith :: forall k v m. (Ord k, (Monoid m, Eq m)) => (MinMax k -> m) -> PQueue k v -> Maybe (v, PQueue k v)
  extractWith pick (PQueue t) =
    let repairWith f maybeResult = fmap (\(v, result) -> (v, f result)) maybeResult
        go (Node2 _ (Leaf l@(Entry (_, v1))) (Leaf r@(Entry (_, v2))))
          | lm <> rm == lm = Just (v1, Pseudo $ leaf r)
          | otherwise = Just (v2, Pseudo $ leaf l)
          where
            lm = pick (measure l :: MinMax k)
            rm = pick (measure r :: MinMax k)
        go (Node3 _ (Leaf l@(Entry (_, v1))) (Leaf m@(Entry (_, v2))) (Leaf r@(Entry (_, v3))))
          | lm <> mm <> rm == lm = Just (v1, Plain $ node2 (leaf m) (leaf r))
          | lm <> mm <> rm == mm = Just (v2, Plain $ node2 (leaf l) (leaf r))
          | otherwise = Just (v3, Plain $ node2 (leaf l) (leaf m))
          where
            lm = pick (measure l :: MinMax k)
            mm = pick (measure m :: MinMax k)
            rm = pick (measure r :: MinMax k)
        go t'@(Node2 _ l r)
          | lm <> rm == lm = repairWith (repairLeftRemove t' r) (go l)
          | otherwise = repairWith (repairRightRemove t' l) (go r)
          where
            lm = pick (measure l :: MinMax k)
            rm = pick (measure r :: MinMax k)
        go t'@(Node3 _ l m r)
          | lm <> mm <> rm == lm = repairWith (repairLeftRemove t' m) (go l)
          | lm <> mm <> rm == mm = repairWith (repairMidRemove t' l) (go m)
          | otherwise = repairWith (repairMidRemove t' m) (go r)
          where
            lm = pick (measure l :: MinMax k)
            mm = pick (measure m :: MinMax k)
            rm = pick (measure r :: MinMax k)
        go (Leaf (Entry (_, v))) = Just (v, Plain $ Empty)
        go Empty = Nothing
     in case go t of
          Just (v, Pseudo t') -> Just (v, PQueue t')
          Just (v, Plain t') -> Just (v, PQueue t')
          _ -> Nothing

  extractMin = extractWith (\(MinMax (m, _)) -> m)
  extractMax = extractWith (\(MinMax (_, m)) -> m)
