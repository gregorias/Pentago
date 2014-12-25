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
  Result(..), 
  PlacementOrder,
  RotationOrder,
  MoveOrder,
  GameState(..),
  SimpleGameState,
  initialSimpleGameState,
  allPlacementOrders,
  allRotationOrders,
  allMoveOrders,
  isFinished,
  getPossibleMoveOrders,
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

type BoardArray = Array (Int, Int) Position

type PlacementOrder = (Int, Int)

type RotationOrder = (Quadrant, RotationDirection)

type MoveOrder = (PlacementOrder, RotationOrder)

data Result = BlackWin | Draw | WhiteWin
  deriving (Eq, Ord, Show)

class GameState s where
  getBoardArray :: (GameState s) => s -> BoardArray
  getPossiblePlacementOrders :: (GameState s) => s -> [PlacementOrder]
  getResult :: (GameState s) => s -> Maybe Result
  makeMove :: (GameState s) => MoveOrder -> s -> s
  whoseTurn :: (GameState s) => s -> Maybe Player

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

isFinished :: (GameState s) => s -> Bool
isFinished = isJust . getResult

getPossibleMoveOrders :: (GameState s) => s -> [MoveOrder]
getPossibleMoveOrders state = do
  pos <- getPossiblePlacementOrders state
  rot <- allRotationOrders
  return (pos, rot)

-- SimpleGameState

data SimpleGameState = SimpleGameState {
  simpleBoardArray :: BoardArray
} deriving (Eq, Ord, Show)

instance GameState SimpleGameState where
  getBoardArray = simpleBoardArray

  getPossiblePlacementOrders = getPositions Empty

  getResult state = mplus 
    (fmap positionToResult (foldl' mplus Nothing [get5InARow curBoard,
      get5InARow (rotate90Matrix curBoard),
      get5Across curBoard,
      get5Across (rotate90Matrix curBoard)]))
    (if length (getPossiblePlacementOrders state) == 0
     then Just Draw
     else Nothing)
    where
      curBoard = simpleBoardArray state

  makeMove (pos, rot) = rotateBoard rot . placeToken pos

  whoseTurn state | count Empty state == 0 = Nothing
                  | count White state == count Black state = Just WhitePlayer
                  | otherwise = Just BlackPlayer

emptyBoard :: BoardArray
emptyBoard = array bounds [(i, Empty) | i <- range bounds]
  where
    bounds = ((0,0), (5, 5))

initialSimpleGameState = SimpleGameState emptyBoard

getPositions :: Position -> SimpleGameState -> [(Int, Int)]
getPositions which state = map fst
  $ filter ((== which) . snd) (assocs $ simpleBoardArray state)

positionToResult Empty = undefined
positionToResult White = WhiteWin
positionToResult Black = BlackWin

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

get5InARow :: BoardArray -> Maybe Position
get5InARow board = foldl' mplus Nothing $ map get5InARow'
  $ map (\i -> subarray ((0, i), (5, i)) board) [0..5]

get5InARow' row = getTheList5SameNonEmptyPositions $ rowToList row

get5Across board = foldl' mplus Nothing $
  map (getTheList5SameNonEmptyPositions . rowToList) [rowA, rowB, rowC]
  where rowA = ixmap (0, 5) (\i -> (i, i)) board
        rowB = ixmap (0, 4) (\i -> (i + 1, i)) board
        rowC = ixmap (0, 4) (\i -> (i, i + 1)) board

count :: Position -> SimpleGameState -> Int
count which = length . getPositions which 

rotateBoard :: RotationOrder -> SimpleGameState -> SimpleGameState
rotateBoard (quadrant, rotationDirection) state =
  SimpleGameState $ insertSubarray newQuadrantMatrix curBoard where
    curBoard = simpleBoardArray state
    quadrantMatrix = subarray (quadrantToBounds quadrant) curBoard
    newQuadrantMatrix =
      rotationDirectionToMatrixSymmetry rotationDirection quadrantMatrix

placeToken :: (Int, Int) -> SimpleGameState -> SimpleGameState
placeToken pos state
  | (curBoard ! pos == Empty) = SimpleGameState $ curBoard // [(pos, turn)]
  | otherwise = undefined
  where
    curBoard = simpleBoardArray state
    turn = if count White state > count Black state
           then Black
           else White

-- Board printing

prettyPrintPosition White = "o"
prettyPrintPosition Black = "x"
prettyPrintPosition Empty = "."

prettyPrintRow :: [Position] -> String
prettyPrintRow row = (foldr (\x a -> "| " ++ (prettyPrintPosition x) ++ " " ++ a) "" row)
  ++ "|"

boardToRows :: BoardArray -> [[Position]]
boardToRows board = map (\i -> elems (subarray ((0, i), (5, i)) board)) [0..5]

-- | Generate a human readable string showing board array's state
prettyShowBoard :: BoardArray -> String
prettyShowBoard board = rowSep ++ (concat $ map (\row -> prettyPrintRow row ++ "\n" ++ rowSep) (boardToRows board))
  where
    rowSep = (take 25 $ cycle ['-']) ++ "\n"
