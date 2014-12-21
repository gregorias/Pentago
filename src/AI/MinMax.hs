module AI.MinMax(
  SMTree, 
  maximize,
  minimize) where

-- ScoreMoveTree
data SMTree a b = SMTreeNode [(b, SMTree a b)] |
                  SMTreeLeaf a 
  deriving (Show)

-- TODO how to eliminate min max duality

maximize :: (Bounded a, Ord a) => SMTree a b -> (a, Maybe b)
maximize smTree = maximize' (minBound) (maxBound) smTree Nothing

minimize :: (Bounded a, Ord a) => SMTree a b -> (a, Maybe b)
minimize smTree = minimize' (minBound) (maxBound) smTree Nothing

maximize' :: (Bounded a, Ord a) => 
  a
  -> a
  -> SMTree a b
  -> Maybe (a, b)
  -> (a, Maybe b)

minimize' :: (Bounded a, Ord a) => 
  a
  -> a
  -> SMTree a b
  -> Maybe (a, b)
  -> (a, Maybe b)

maximize' alpha beta (SMTreeLeaf score) _ = (score, Nothing)

maximize' alpha beta (SMTreeNode []) Nothing = undefined

maximize' alpha beta (SMTreeNode []) (Just (score, move)) = (score, Just move)

maximize' alpha beta (SMTreeNode ((move, childTree):xs)) acc =
  if score >= beta
  then (score, Just move)
  else maximize' newAlpha beta (SMTreeNode xs) newAcc
  where
    (score, _) = minimize' alpha beta childTree Nothing
    newAcc = case acc of
      Nothing -> Just (score, move)
      Just (accScore, _) -> if score > accScore
        then Just (score, move)
        else acc
    newAlpha = max alpha score

minimize' alpha beta (SMTreeLeaf score) _ = (score, Nothing)

minimize' alpha beta (SMTreeNode []) Nothing = undefined

minimize' alpha beta (SMTreeNode []) (Just (score, move)) = (score, Just move)

minimize' alpha beta (SMTreeNode ((move, childTree):xs)) acc =
  if score <= alpha
  then (score, Just move)
  else minimize' alpha newBeta (SMTreeNode xs) newAcc
  where
    (score, _) = maximize' alpha beta childTree Nothing
    newAcc = case acc of
      Nothing -> Just (score, move)
      Just (accScore, _) -> if score > accScore
        then Just (score, move)
        else acc
    newBeta = min beta score
