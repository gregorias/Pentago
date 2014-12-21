module Pentago.AI.Pentago where

import Pentago.AI.MinMax
import Pentago.Data.Matrix
import Pentago.Data.Pentago
import Pentago.Data.Tree

import Control.Applicative
import Data.Foldable
import Data.Function
import Data.List
import Data.Monoid
import Data.Tuple
import Data.Traversable
import qualified Data.Set

type PentagoGameTree = EdgeTree MoveOrder Board

generatePentagoGameTree :: Board -> PentagoGameTree
generatePentagoGameTree board
  | isFinished board = ValueNode board []
  | otherwise = ValueNode board (map (fmap generatePentagoGameTree)
    uniqueChildBoardsWithMoves)
  where 
    possibleMoveOrders = generatePossibleMoveOrders board
    getBoard = fst
    childBoardsWithMoves = map
      (\moveOrder -> (moveOrder, makeMove moveOrder board))
      possibleMoveOrders
    uniqueChildBoardsWithMoves = nubBy ((==) `on` snd)
      . sortBy (compare `on` snd)
      $ childBoardsWithMoves

sizeOfGameTree :: PentagoGameTree -> Int
sizeOfGameTree = Data.Foldable.foldl' (+) 0 . fmap (const 1)

prune :: Int -> PentagoGameTree -> PentagoGameTree
prune 0 (ValueNode a xs) = ValueNode a []
prune d (ValueNode a xs) = ValueNode a $ map (fmap $ prune (d - 1)) xs

type Score = Float

type PentagoEvalutionTree = LeafValueTree MoveOrder Score

evaluateTree :: (Applicative f) => (Board -> f Score) -> PentagoGameTree -> f PentagoEvalutionTree
evaluateTree evaluateF gameTree = traverse evaluateF leafTree
  where
    leafTree = toLeafValueTree gameTree

-- trivialEvaluate :: Board  Board -> PentagoEvalutionTree
--evaluateTree' board = case getResult board of
  --Nothing -> 0.0
  --Just WhiteWin -> 1.0
  --Just BlackWin -> -1.0

-- simpleEvaluateBoard = 
