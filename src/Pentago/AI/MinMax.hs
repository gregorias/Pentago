module Pentago.AI.MinMax(
  SMTree, 
  maximize,
  minimize) where

import Pentago.Data.Tree

-- ScoreMoveTree
type SMTree e v = LeafValueTree e v

-- TODO how to eliminate min max duality

maximize :: (Bounded v, Ord v) => SMTree e v -> (v, Maybe e)
maximize smTree = maximize' (minBound) (maxBound) smTree Nothing

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

maximize' alpha beta (Leaf score) _ = (score, Nothing)

maximize' alpha beta (Node []) Nothing = undefined

maximize' alpha beta (Node []) (Just (score, move)) = (score, Just move)

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

minimize' alpha beta (Leaf score) _ = (score, Nothing)

minimize' alpha beta (Node []) Nothing = undefined

minimize' alpha beta (Node []) (Just (score, move)) = (score, Just move)

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
