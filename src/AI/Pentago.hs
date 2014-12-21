module AI.Pentago where

import Data.List
import Data.Matrix
import Data.Pentago
import Data.Tuple
import AI.MinMax

data PentagoGameTree = Node Board [(PentagoGameTree, MoveOrder)]
  deriving (Show)

generatePentagoGameTree :: Board -> PentagoGameTree
generatePentagoGameTree board 
  | isFinished board = Node board []
  | otherwise = Node board (map (swap . fmap generatePentagoGameTree . swap)
    uniqueChildBoardsWithMoves)
  where 
    childBoardsWithMoves = map
      (\moveOrder -> (makeMove moveOrder board, moveOrder))
      (generatePossibleMoveOrders board)
    uniqueChildBoardsWithMoves = nubBy (\x y -> (fst x) == (fst y))
      . sortBy (\x y -> compare (fst x) (fst y))
      $ childBoardsWithMoves

sizeOfGameTree :: PentagoGameTree -> Int
sizeOfGameTree (Node _ []) = 1
sizeOfGameTree (Node _ xs) = 1 + (foldl'
  (\a x -> let z = (sizeOfGameTree . fst) x in seq z (a + z)) 0 xs)

prune :: Int -> PentagoGameTree -> PentagoGameTree
prune 0 (Node a _) = Node a []
prune d tree@(Node a []) = tree
prune d (Node a xs) = Node a $ map (swap . (fmap $ prune (d - 1)) . swap) xs

type Score = Float

evaluate :: Board -> Score
evaluate board = case getResult board of
  Nothing -> 0.0
  Just WhiteWin -> 1.0
  Just BlackWin -> -1.0
