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
  UnboxedGameState,
  initialUnboxedGameState,
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
import Data.Array.Unboxed
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

rotationDirectionToMatrixSymmetry :: (Ix i, Integral i, IArray a e) =>
  RotationDirection -> MatrixSymmetry a i e
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

-- Array board helper functions

findArrayElements :: (Eq e, Ix i, IArray a e) => e -> a i e -> [i]
findArrayElements which array =
  map fst $ filter ((== which) . snd) (assocs array)

count :: (Eq e, Ix i, IArray a e) => e -> a i e -> Int
count which = length . findArrayElements which

rowToList row = map snd (assocs row)

getRepeatingElement :: (Eq e) => [e] -> Maybe e
getRepeatingElement [] = Nothing
getRepeatingElement xs = 
  if all id $ map (== (head xs)) (tail xs)
  then Just $ head xs
  else Nothing

split6ElementList :: [a] -> (a, [a], a)
split6ElementList (first:xs) = (first, center, last)
  where (center, [last]) = splitAt 4 xs

get5RepeatingElementsFrom6ElementList :: (Eq e) => [e] -> Maybe e
get5RepeatingElementsFrom6ElementList xs = do
  e <- getRepeatingElement center
  if first == e
  then return e
  else if last == e
  then return e
  else fail "No repeating element found"
  where (first, center, last) = split6ElementList xs

hasBoardRowSameElements :: (Eq e, IArray a e)
  => a Int e -> Maybe e
hasBoardRowSameElements board = 
  if hasTheCenterSameElements
  then 
    if first == second
    then Just first
    else if last == second
    then Just last
    else Nothing
  else Nothing
  where
    first = board ! 0
    second = board ! 1
    last = board ! 5
    hasTheCenterSameElements = and $ map
      (\idx -> second == board ! idx) [2..4]

getTheListSamePositions :: (Eq a) => a -> [a] -> Maybe a
getTheListSamePositions _ [] = Nothing
getTheListSamePositions except xs = 
  if all id $ map ((&&) <$> (== (head xs)) <*> (/= except)) (tail xs)
  then Just $ head xs
  else Nothing

getTheList5SamePositions :: (Eq a) => a -> [a] -> Maybe a
getTheList5SamePositions _ [] = Nothing
getTheList5SamePositions except (x:xs) =
  if length first5 < 5
  then Nothing
  else getTheListSamePositions except first5
    `mplus` getTheList5SamePositions except xs
  where first5 = take 5 (x:xs)

get5InARow :: (Eq e, IArray a e) => e -> a (Int, Int) e -> Maybe e
get5InARow except board = foldl' mplus Nothing
  $ map (get5InARow' except)
  $ map (\i -> subarray ((0, i), (5, i)) board) [0..5]

betterGet5InARow :: (Eq e, IArray a e) => e -> a (Int, Int) e -> Maybe e
betterGet5InARow except board = foldl' mplus Nothing 
  $ map (betterGet5InARow' except)
  $ map (\y -> (ixmap (0, 5) (\x -> (x, y)) board))  [0..5]

get5InAColumn :: (Eq e, IArray a e) => e -> a (Int, Int) e -> Maybe e
get5InAColumn except board = foldl' mplus Nothing $ map (get5InARow' except)
  $ map (\i -> subarray ((i, 0), (i, 5)) board) [0..5]

betterGet5InAColumn :: (Eq e, IArray a e) => e -> a (Int, Int) e -> Maybe e
betterGet5InAColumn except board = foldl' mplus Nothing
  $ map (betterGet5InARow' except)
  $ map (\x -> (ixmap (0, 5) (\y -> (x, y)) board)) [0..5]

get5InARow' except row = do
  elem <- get5RepeatingElementsFrom6ElementList (rowToList row)
  if elem /= except
  then return elem
  else fail "Found only empty element"

betterGet5InARow' except row = do
  elem <- hasBoardRowSameElements row
  if elem /= except
  then return elem
  else fail "Found only empty element"

get5LRAcross except board = foldl' mplus Nothing $
  map (getTheList5SamePositions except . rowToList) [rowA, rowB, rowC]
  where rowA = ixmap (0, 5) (\i -> (i, i)) board
        rowB = ixmap (0, 4) (\i -> (i + 1, i)) board
        rowC = ixmap (0, 4) (\i -> (i, i + 1)) board

get5RLAcross except board = foldl' mplus Nothing $
  map (getTheList5SamePositions except . rowToList) [rowA, rowB, rowC]
  where rowA = ixmap (0, 5) (\i -> (5 - i, i)) board
        rowC = ixmap (0, 4) (\i -> (4 - i, i)) board
        rowB = ixmap (0, 4) (\i -> (5 - i,  i + 1)) board

rotateQuadrant :: (IArray a e) => RotationOrder 
  -> a (Int, Int) e
  -> a (Int, Int) e
rotateQuadrant (quadrant, rotationDirection) board =
  insertSubarray newQuadrantMatrix board where
    quadrantMatrix = subarray (quadrantToBounds quadrant) board
    newQuadrantMatrix =
      rotationDirectionToMatrixSymmetry rotationDirection quadrantMatrix

placeToken :: (Ix i, IArray a e) => i -> e -> a i e -> a i e
placeToken pos elem board = board // [(pos, elem)]

-- SimpleGameState

-- | GameState which uses boxed array as board representation
data SimpleGameState = SimpleGameState {
  simpleBoardArray :: BoardArray
} deriving (Eq, Ord, Show)

instance GameState SimpleGameState where
  getBoardArray = simpleBoardArray

  getPossiblePlacementOrders = findArrayElements Empty . getBoardArray

  getResult state = mplus 
    (fmap positionToResult (foldl' mplus Nothing [
      betterGet5InARow Empty curBoard,
      betterGet5InAColumn Empty curBoard,
      get5LRAcross Empty curBoard,
      get5RLAcross Empty curBoard]))
    (if length (getPossiblePlacementOrders state) == 0
     then Just Draw
     else Nothing)
    where
      curBoard = simpleBoardArray state

  makeMove (pos, rot) state =
    if curBoard ! pos == Empty
    then SimpleGameState $ rotateQuadrant rot . placeToken pos token $ curBoard
    else undefined
    where 
      currentTurn = whoseTurn state
      curBoard = simpleBoardArray state
      token = case currentTurn of
                Just WhitePlayer -> White
                Just BlackPlayer -> Black

  whoseTurn state | count Empty curBoard == 0 = Nothing
                  | count White curBoard == count Black curBoard
                    = Just WhitePlayer
                  | otherwise = Just BlackPlayer
                  where curBoard = getBoardArray state

emptyBoard :: (IArray a e) => e -> a (Int, Int) e
emptyBoard emptyValue = array bounds [(i, emptyValue) | i <- range bounds]
  where
    bounds = ((0,0), (5, 5))

initialSimpleGameState = SimpleGameState (emptyBoard Empty)

positionToResult Empty = undefined
positionToResult White = WhiteWin
positionToResult Black = BlackWin

-- UnboxedGameState
--
-- | GameState which uses unboxed array as board representation

type UnboxedBoardArray = UArray (Int, Int) Char

charToPosition '.' = Empty
charToPosition 'o' = White
charToPosition 'x' = Black

positionToChar Empty = '.'
positionToChar White = 'o'
positionToChar Black = 'x'

unboxedToBoxedBoardArray :: UnboxedBoardArray -> BoardArray
unboxedToBoxedBoardArray unboxed = array
  arrayBounds [(i, pos) | i <- range arrayBounds,
    let pos = charToPosition $ unboxed ! i]
  where
    arrayBounds = bounds unboxed

data UnboxedGameState = UnboxedGameState {
  unboxedBoardArray :: UnboxedBoardArray
} deriving (Eq, Ord, Show)

instance GameState UnboxedGameState where
  getBoardArray = unboxedToBoxedBoardArray . unboxedBoardArray

  getPossiblePlacementOrders =
    findArrayElements (positionToChar Empty) . unboxedBoardArray

  getResult state = mplus
    (fmap (positionToResult . charToPosition) (foldl' mplus Nothing [
      betterGet5InARow emptyChar curBoard,
      betterGet5InAColumn emptyChar curBoard,
      get5LRAcross emptyChar curBoard,
      get5RLAcross emptyChar curBoard]))
    (if length (getPossiblePlacementOrders state) == 0
     then Just Draw
     else Nothing)
    where
      curBoard = unboxedBoardArray state
      emptyChar = positionToChar Empty

  makeMove (pos, rot) state =
    if curBoard ! pos == positionToChar Empty
    then UnboxedGameState
      $ rotateQuadrant rot . placeToken pos token
      $ curBoard
    else undefined
    where 
      currentTurn = whoseTurn state
      curBoard = unboxedBoardArray state
      token = case currentTurn of
                Just WhitePlayer -> (positionToChar White)
                Just BlackPlayer -> (positionToChar Black)

  whoseTurn state | count (positionToChar Empty) curBoard == 0 = Nothing
                  | count (positionToChar White) curBoard
                    == count (positionToChar Black) curBoard = Just WhitePlayer
                  | otherwise = Just BlackPlayer
                  where curBoard = unboxedBoardArray state

initialUnboxedGameState = UnboxedGameState (emptyBoard . positionToChar $ Empty)

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
