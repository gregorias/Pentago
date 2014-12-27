{-|
Module : Pentago.AI.MinMax
Description : Implementations of alfa-beta minmax algorithm functions

Implementations of alfa-beta minmax algorithm functions
-}
module Pentago.AI.MinMax(
  SMTree, 
  maximize,
  minimize) where

import Pentago.Data.Tree

-- | ScoreMoveTree
type SMTree e v = LeafValueTree e v

-- | Find a move which maximizes eventual score value
maximize :: (Bounded v, Ord v) => SMTree e v -> (v, Maybe e)
maximize smTree = maximize' (minBound) (maxBound) smTree Nothing

-- | Find a move which minimizes eventual score value
minimize :: (Bounded v, Ord v) => SMTree e v -> (v, Maybe e)
minimize smTree = minimize' (minBound) (maxBound) smTree Nothing

maximize' :: (Bounded v, Ord v) => 
  v
  -> v
  -> SMTree e v
  -> Maybe (v, e)
  -> (v, Maybe e)

minimize' :: (Bounded v, Ord v) => 
  v
  -> v
  -> SMTree e v
  -> Maybe (v, e)
  -> (v, Maybe e)

maximize' _ _ (Leaf score) _ = (score, Nothing)

maximize' _ _ (Node []) Nothing = undefined

maximize' _ _ (Node []) (Just (score, move)) = (score, Just move)

maximize' alpha beta (Node ((move, childTree):xs)) acc =
  if score >= beta
  then (score, Just move)
  else maximize' newAlpha beta (Node xs) newAcc
  where
    (score, _) = minimize' alpha beta childTree Nothing
    newAcc = case acc of
      Nothing -> Just (score, move)
      Just (accScore, _) -> if score > accScore
        then Just (score, move)
        else acc
    newAlpha = max alpha score

minimize' _ _ (Leaf score) _ = (score, Nothing)

minimize' _ _ (Node []) Nothing = undefined

minimize' _ _ (Node []) (Just (score, move)) = (score, Just move)

minimize' alpha beta (Node ((move, childTree):xs)) acc =
  if score <= alpha
  then (score, Just move)
  else minimize' alpha newBeta (Node xs) newAcc
  where
    (score, _) = maximize' alpha beta childTree Nothing
    newAcc = case acc of
      Nothing -> Just (score, move)
      Just (accScore, _) -> if score < accScore
        then Just (score, move)
        else acc
    newBeta = min beta score
