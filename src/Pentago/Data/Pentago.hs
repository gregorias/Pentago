module Pentago.Data.Pentago(
  Player(..),
  Position(..),
  Quadrant(..),
  RotationDirection(..),
  Board,
  PlacementOrder,
  RotationOrder,
  MoveOrder,
  Result(..), 
  allPlacementOrders,
  allRotationOrders,
  allMoveOrders,
  emptyBoard,
  rotateBoard,
  placeToken,
  makeMove,
  whoseTurn,
  isFinished,
  getResult,
  generatePossiblePlacementOrders,
  generatePossibleMoveOrders,
  prettyShowBoard
  ) where

import Pentago.Data.Matrix

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Ix
import Data.List
import Data.Maybe

data Player = BlackPlayer | WhitePlayer
  deriving (Eq, Ord, Show)

data Position = Empty | Black | White
  deriving (Eq, Ord, Show)

data Quadrant = RightTop | LeftTop | LeftBottom | RightBottom
  deriving (Eq, Ord, Show)

data RotationDirection = LeftRotation | RightRotation
  deriving (Eq, Ord, Show)

type Board = Array (Int, Int) Position

type PlacementOrder = (Int, Int)

type RotationOrder = (Quadrant, RotationDirection)

type MoveOrder = (PlacementOrder, RotationOrder)

data Result = BlackWin | Draw | WhiteWin
  deriving (Eq, Ord, Show)

quadrantToBounds :: Quadrant -> ((Int, Int), (Int, Int))
quadrantToBounds RightTop = ((3, 0), (5, 2))
quadrantToBounds LeftTop = ((0, 0), (2, 2))
quadrantToBounds LeftBottom = ((0, 3), (2, 5))
quadrantToBounds RightBottom = ((3, 3), (5, 5))

rotationDirectionToMatrixSymmetry :: (Ix i, Integral i) =>
  RotationDirection -> MatrixSymmetry i e
rotationDirectionToMatrixSymmetry LeftRotation = rotate90Matrix
rotationDirectionToMatrixSymmetry RightRotation = rotate270Matrix

allPlacementOrders = range ((0,0), (5,5))

allRotationOrders = do
  x <- [RightTop, LeftTop, LeftBottom, RightBottom]
  y <- [LeftRotation, RightRotation]
  return (x,y)

allMoveOrders = [(p, r) | p <- allPlacementOrders, r <- allRotationOrders]

emptyBoard :: Board
emptyBoard = array bounds [(i, Empty) | i <- range bounds]
  where
    bounds = ((0,0), (5, 5))

getPositions :: Position -> Board -> [(Int, Int)]
getPositions which board = map fst $ filter ((== which) . snd) (assocs board)

count :: Position -> Board -> Int
count which = length . getPositions which 

rotateBoard :: RotationOrder -> Board -> Board
rotateBoard (quadrant, rotationDirection) board =
  insertSubarray newQuadrantMatrix board where
    quadrantMatrix = subarray (quadrantToBounds quadrant) board
    newQuadrantMatrix =
      rotationDirectionToMatrixSymmetry rotationDirection quadrantMatrix

placeToken :: (Int, Int) -> Board -> Board
placeToken pos board 
  | (board ! pos == Empty) = board // [(pos, whoseTurn)]
  | otherwise = undefined
  where
    whoseTurn = if count White board > count Black board then Black else White

whoseTurn :: Board -> Maybe Player
whoseTurn board | count Empty board == 0 = Nothing
                | count White board == count Black board = Just WhitePlayer
                | otherwise = Just BlackPlayer

makeMove :: MoveOrder -> Board -> Board
makeMove (pos, rot) = rotateBoard rot . placeToken pos

getTheListSameNonEmptyPositions :: [Position] -> Maybe Position
getTheListSameNonEmptyPositions [] = Nothing
getTheListSameNonEmptyPositions xs = 
  if all id $ map ((&&) <$> (== (head xs)) <*> (/= Empty)) (tail xs)
  then Just $ head xs
  else Nothing

getTheList5SameNonEmptyPositions :: [Position] -> Maybe Position
getTheList5SameNonEmptyPositions [] = Nothing
getTheList5SameNonEmptyPositions (x:xs) =
  if length first5 < 5
  then Nothing
  else getTheListSameNonEmptyPositions first5
    `mplus` getTheList5SameNonEmptyPositions xs
  where first5 = take 5 (x:xs)

rowToList row = map snd (assocs row)

get5InARow :: Board -> Maybe Position
get5InARow board = foldl' mplus Nothing $ map get5InARow'
  $ map (\i -> subarray ((0, i), (5, i)) board) [0..5]

get5InARow' row = getTheList5SameNonEmptyPositions $ rowToList row

get5Across board = foldl' mplus Nothing $
  map (getTheList5SameNonEmptyPositions . rowToList) [rowA, rowB, rowC]
  where rowA = ixmap (0, 5) (\i -> (i, i)) board
        rowB = ixmap (0, 4) (\i -> (i + 1, i)) board
        rowC = ixmap (0, 4) (\i -> (i, i + 1)) board

isFinished :: Board -> Bool
isFinished = isJust . getResult

positionToResult Empty = undefined
positionToResult White = WhiteWin
positionToResult Black = BlackWin

getResult :: Board -> Maybe Result
getResult board = mplus 
  (fmap positionToResult (foldl' mplus Nothing [get5InARow board,
    get5InARow (rotate90Matrix board),
    get5Across board,
    get5Across (rotate90Matrix board)]))
  (if length (generatePossiblePlacementOrders board) == 0
   then Just Draw
   else Nothing)

generatePossiblePlacementOrders :: Board -> [PlacementOrder]
generatePossiblePlacementOrders = getPositions Empty

generatePossibleMoveOrders :: Board -> [MoveOrder]
generatePossibleMoveOrders board = do
  pos <- generatePossiblePlacementOrders board
  rot <- allRotationOrders
  return (pos, rot)

prettyPrintPosition White = "o"
prettyPrintPosition Black = "x"
prettyPrintPosition Empty = "."

prettyPrintRow :: [Position] -> String
prettyPrintRow row = (foldr (\x a -> "| " ++ (prettyPrintPosition x) ++ " " ++ a) "" row)
  ++ "|"

boardToRows :: Board -> [[Position]]
boardToRows board = map (\i -> elems (subarray ((0, i), (5, i)) board)) [0..5]
  
prettyShowBoard :: Board -> String
prettyShowBoard board = rowSep ++ (concat $ map (\row -> prettyPrintRow row ++ "\n" ++ rowSep) (boardToRows board))
  where
    rowSep = (take 25 $ cycle ['-']) ++ "\n"
