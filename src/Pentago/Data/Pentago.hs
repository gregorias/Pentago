{-|
Module : Pentago.Data.Pentago
Description : Types and operations representing a Pentago game

Types and operations representing a Pentago game
-}
module Pentago.Data.Pentago(
  Player(..),
  Position(..),
  Quadrant(..),
  RotationDirection(..),
  Board,
  GameState(board),
  PlacementOrder,
  RotationOrder,
  MoveOrder,
  Result(..), 
  allPlacementOrders,
  allRotationOrders,
  allMoveOrders,
  makeMove,
  initialGameState,
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

data GameState = GameState {
  board :: Board
} deriving (Eq, Ord, Show)

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

initialGameState :: GameState
initialGameState = GameState emptyBoard

getPositions :: Position -> GameState -> [(Int, Int)]
getPositions which state = map fst
  $ filter ((== which) . snd) (assocs $ board state)

count :: Position -> GameState -> Int
count which = length . getPositions which 

rotateBoard :: RotationOrder -> GameState -> GameState
rotateBoard (quadrant, rotationDirection) state =
  GameState $ insertSubarray newQuadrantMatrix curBoard where
    curBoard = board state
    quadrantMatrix = subarray (quadrantToBounds quadrant) curBoard
    newQuadrantMatrix =
      rotationDirectionToMatrixSymmetry rotationDirection quadrantMatrix

placeToken :: (Int, Int) -> GameState -> GameState
placeToken pos state
  | (curBoard ! pos == Empty) = GameState $ curBoard // [(pos, whoseTurn)]
  | otherwise = undefined
  where
    curBoard = board state
    whoseTurn = if count White state > count Black state
                then Black
                else White

whoseTurn :: GameState -> Maybe Player
whoseTurn state | count Empty state == 0 = Nothing
                | count White state == count Black state = Just WhitePlayer
                | otherwise = Just BlackPlayer

makeMove :: MoveOrder -> GameState -> GameState
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

isFinished :: GameState -> Bool
isFinished = isJust . getResult

positionToResult Empty = undefined
positionToResult White = WhiteWin
positionToResult Black = BlackWin

getResult :: GameState -> Maybe Result
getResult state = mplus 
  (fmap positionToResult (foldl' mplus Nothing [get5InARow curBoard,
    get5InARow (rotate90Matrix curBoard),
    get5Across curBoard,
    get5Across (rotate90Matrix curBoard)]))
  (if length (generatePossiblePlacementOrders state) == 0
   then Just Draw
   else Nothing)
  where
    curBoard = board state

generatePossiblePlacementOrders :: GameState -> [PlacementOrder]
generatePossiblePlacementOrders = getPositions Empty

generatePossibleMoveOrders :: GameState -> [MoveOrder]
generatePossibleMoveOrders state = do
  pos <- generatePossiblePlacementOrders state
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
