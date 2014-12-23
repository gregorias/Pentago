module Pentago.Data.Tree(
  EdgeTree(..),
  LeafValueTree(..),
  toLeafValueTree) where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable

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

instance Foldable (LeafValueTree e) where
  foldMap f (Leaf v) = f v
  foldMap f (Node xs) = (mconcat $ map (foldMap f . snd) xs)

instance Traversable (LeafValueTree e) where
  sequenceA (Leaf fv) = Leaf <$> fv
  sequenceA (Node xs) = Node <$> sequenceA fList -- xs :: [(e, T e (f v))]
    where 
      efTList = map (fmap sequenceA) xs -- [(e, f T e v)]
      fList = map (\(e, fT) -> (\t -> (e, t)) <$> fT) efTList -- f [(e, T e v)]

toLeafValueTree :: EdgeTree e v -> LeafValueTree e v
toLeafValueTree (ValueNode v []) = Leaf v
toLeafValueTree (ValueNode v xs) = Node $ (fmap . fmap $ toLeafValueTree) xs
