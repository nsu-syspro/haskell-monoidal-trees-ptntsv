{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task4.Tree where

import Common.MonoidalTree
import Data.Foldable (Foldable (toList))
-- import Data.Foldable1 (Foldable1 (foldMap1, toNonEmpty))
-- import Data.List.NonEmpty (NonEmpty (..), fromList, head, tail)
import Task1 (Measured (..))
import Prelude hiding (head, tail)

-- * Finger tree definition

-- | Finger tree with values 'a' in leaves
-- Intermediate branches contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Single a
  | Deep m (Digit a) (Tree m (Node m a)) (Digit a)
  deriving (Show, Eq)

-- | 2-3 node of finger tree
data Node m a
  = Node2 m a a
  | Node3 m a a a
  deriving (Show, Eq)

-- | Finger tree digit
data Digit a
  = One a
  | Two a a
  | Three a a a
  | Four a a a a
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance (Measured m a) => Measured m (Tree m a) where
  measure Empty = mempty
  measure (Single x) = measure x
  measure (Deep m _ _ _) = m

-- | Measures given node using provided measure of 'a'
instance (Measured m a) => Measured m (Node m a) where
  measure (Node2 m _ _) = m
  measure (Node3 m _ _ _) = m

-- | Measures given digit using provided measure of 'a'
instance {-# INCOHERENT #-} (Measured m a) => Measured m (Digit a) where
  measure = foldMap measure

instance Foldable (Tree m) where
  foldMap _ Empty = mempty
  foldMap f (Single x) = f x
  foldMap f (Deep _ d1 t d2) = foldMap f d1 <> foldMap (foldMap f) t <> foldMap f d2

instance Foldable (Node m) where
  foldMap f (Node2 _ l r) = f l <> f r
  foldMap f (Node3 _ l m r) = f l <> f m <> f r

instance Foldable Digit where
  foldMap f (One x1) = f x1
  foldMap f (Two x1 x2) = f x1 <> f x2
  foldMap f (Three x1 x2 x3) = f x1 <> f x2 <> f x3
  foldMap f (Four x1 x2 x3 x4) = f x1 <> f x2 <> f x3 <> f x4

-- * Smart constructors

single :: a -> Tree m a
single = Single

node2 :: (Measured m a) => a -> a -> Node m a
node2 l r = Node2 (measure l <> measure r) l r

node3 :: (Measured m a) => a -> a -> a -> Node m a
node3 l m r = Node3 (measure l <> measure m <> measure r) l m r

deep :: forall m a. (Measured m a) => Digit a -> Tree m (Node m a) -> Digit a -> Tree m a
deep pr m sf = Deep (measure pr <> measure m <> measure sf) pr m sf

data LView s a = LNil | LCons a (s a)
  deriving (Show, Eq)

viewl :: forall m a. (Measured m a) => Tree m a -> LView (Tree m) a
viewl Empty = LNil
viewl (Single x) = LCons x Empty
viewl (Deep _ pr m sf) = case toList pr of
  (p : pr') -> LCons p (deepl pr' m sf)
  _ -> error "unexpected empty Digit in viewl"

toDigitUnsafe :: [a] -> Digit a
toDigitUnsafe [x1] = One x1
toDigitUnsafe [x1, x2] = Two x1 x2
toDigitUnsafe [x1, x2, x3] = Three x1 x2 x3
toDigitUnsafe [x1, x2, x3, x4] = Four x1 x2 x3 x4
toDigitUnsafe _ = error "unexpected empty Digit in toDigitUnsafe"

deepl :: forall m a. (Measured m a) => [a] -> Tree m (Node m a) -> Digit a -> Tree m a
deepl [] m sf = case viewl m of
  LNil -> toTree sf
  LCons l m' -> deep (toDigitUnsafe $ toList l) m' sf
  where

deepl pr m sf = deep (toDigitUnsafe $ toList pr) m sf

-- deepSafeL Nothing m sf = case viewL

-- * Monoidal tree instance

instance MonoidalTree Tree where
  toTree = foldr (<|) Empty
  (<|) x Empty = Single x
  (<|) x (Single x1) = deep (One x) Empty (One x1)
  (<|) x (Deep _ (One x1) m sf) = deep (Two x x1) m sf
  (<|) x (Deep _ (Two x1 x2) m sf) = deep (Three x x1 x2) m sf
  (<|) x (Deep _ (Three x1 x2 x3) m sf) = deep (Four x x1 x2 x3) m sf
  (<|) x (Deep _ (Four x1 x2 x3 x4) m sf) = deep (Two x x1) ((node3 x2 x3 x4) <| m) sf

  (|>) Empty x = Single x
  (|>) (Single x1) x = deep (One x1) Empty (One x)
  (|>) (Deep _ pr m (One x1)) x = deep pr m (Two x1 x)
  (|>) (Deep _ pr m (Two x1 x2)) x = deep pr m (Three x1 x2 x)
  (|>) (Deep _ pr m (Three x1 x2 x3)) x = deep pr m (Four x1 x2 x3 x)
  (|>) (Deep _ pr m (Four x1 x2 x3 x4)) x = deep pr (m |> (node3 x1 x2 x3)) (Two x4 x)

-- * Utility functions

-- | Split result with left part, middle element and right part
data Split f a = Split (f a) a (f a)
  deriving (Show, Eq)

splitDigit :: forall m a. (Measured m a) => (m -> Bool) -> m -> Digit a -> Split [] a
splitDigit p i digit = let Split l x r = (splitDigit' i (toList digit)) in Split l x r
  where
    splitDigit' :: m -> [a] -> Split [] a
    splitDigit' _ [] = error "unreachable"
    splitDigit' _ [x] = Split [] x []
    splitDigit' acc (x : xs)
      | p acc' = Split [] x xs
      | otherwise = let Split l y r = splitDigit' acc' xs in Split (x : l) y r
      where
        acc' = acc <> (measure x :: m)

-- | Helper function for spliting tree based on given predicate and starting accumulator value
splitTree :: (Measured m a) => (m -> Bool) -> m -> Tree m a -> Maybe (Split (Tree m) a)
splitTree _ _ Empty = Nothing
splitTree _ _ (Single a) = Just (Split Empty a Empty)
splitTree p acc (Deep _ pr m sf)
  | p mpr = let Split lpr x rpr = splitDigit p acc pr in Just (Split (toTree lpr) x (deepl rpr m sf))
  where
    mpr = acc <> measure pr
-- mm = acc <> measure m
splitTree _ _ _ = error "todo"

-- | Splits tree based on given predicate
split :: (Measured m a) => (m -> Bool) -> Tree m a -> (Tree m a, Tree m a)
split = error "TODO: define split"

-- | Concatenates two trees
infixr 6 ><

(><) :: (Measured m a) => Tree m a -> Tree m a -> Tree m a
(><) = error "TODO: define (><)"
