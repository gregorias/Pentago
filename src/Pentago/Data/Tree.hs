{-|
Module : Pentago.Data.Tree
Description : Basic tree-like data types

Basic tree-like data types
-}
module Pentago.Data.Tree(
  EdgeTree(..),
  LeafValueTree(..),
  toLeafValueTree) where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable

-- |Tree with edges
data EdgeTree e v = ValueNode v [(e, EdgeTree e v)]
  deriving (Show) 

-- |Tree with values only in leaves
data LeafValueTree e v = Node [(e, LeafValueTree e v)] |
                         Leaf v

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

instance (Show e, Show v) => Show (LeafValueTree e v) where
  show = showWithPrefix ""
    where
      showWithPrefix :: (Show e, Show v) => String -> LeafValueTree e v -> String
      showWithPrefix prefix (Leaf v) = prefix ++ show v ++ "\n"
      showWithPrefix prefix (Node xs) = 
        Prelude.foldr
          (\(e, subtree) a ->
            (edgeString e) ++ showWithPrefix newPrefix subtree ++ a)
          ""
          xs
        where
          newPrefix = prefix ++ "  "
          edgeString e = prefix ++ show e ++ "\n"
         

-- |Transform EdgeTree to LeafValueTree tree discarding inner node values.
toLeafValueTree :: EdgeTree e v -> LeafValueTree e v
toLeafValueTree (ValueNode v []) = Leaf v
toLeafValueTree (ValueNode v xs) = Node $ (fmap . fmap $ toLeafValueTree) xs
