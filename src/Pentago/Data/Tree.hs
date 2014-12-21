module Pentago.Data.Tree where

import Data.Foldable
import Data.Monoid

data EdgeTree e v = ValueNode v [(e, EdgeTree e v)]
  deriving (Show) 

data LeafValueTree e v = Node [(e, LeafValueTree e v)] |
                         Leaf v
  deriving (Show) 

instance Functor (EdgeTree e) where
  fmap f (ValueNode v xs) = ValueNode (f v) ((fmap . fmap . fmap $ f) xs)

instance Foldable (EdgeTree e) where
  foldMap f (ValueNode v xs) =
    mappend (f v) (mconcat $ map (foldMap f . snd) xs)

instance Functor (LeafValueTree e) where
  fmap f (Leaf v) = Leaf $ f v
  fmap f (Node xs) = Node ((fmap . fmap . fmap $ f) xs)

toLeafValueTree :: EdgeTree e v -> LeafValueTree e v
toLeafValueTree (ValueNode v []) = Leaf v
toLeafValueTree (ValueNode v xs) = Node $ (fmap . fmap $ toLeafValueTree) xs
