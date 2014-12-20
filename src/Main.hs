module Main where
import Control.Applicative
import Control.Monad
import Data.Array
import Data.Ix
import Data.List
import Data.Maybe
import Data.Tuple
import System.Random

----- Data. Matrix
type Symmetry i = (i, i, Bool) -> (i, i) -> (i, i)

-- s
horizontalSymmetry :: (Integral i) => Symmetry i
horizontalSymmetry (_, cY, True) = fmap (2 * cY + 1 -)
horizontalSymmetry (_, cY, _) = fmap (2 * cY -)

-- sr
transposeSymmetry :: (Integral i) => Symmetry i
transposeSymmetry (cX, cY, _) (x, y) = (cX + (y - cY), cY + (x - cX))

-- sr^2
verticalSymmetry :: (Integral i) => Symmetry i
verticalSymmetry center = swap . horizontalSymmetry (mySwap center) . swap 
  where mySwap (x, y, c) = (y, x, c)

-- r^2
rotate180Symmetry :: (Integral i) => Symmetry i
rotate180Symmetry center = horizontalSymmetry center . verticalSymmetry center

-- r
rotate90Symmetry :: (Integral i) => Symmetry i
rotate90Symmetry center = horizontalSymmetry center . transposeSymmetry center

-- r^3
rotate270Symmetry :: (Integral i) => Symmetry i
rotate270Symmetry center = transposeSymmetry center . horizontalSymmetry center

-- TODO quickcheck that transpose . transpose = id

type MatrixSymmetry i e = Array (i, i) e -> Array (i,i) e

matrixSymmetry :: (Ix i, Integral i) => Symmetry i -> MatrixSymmetry i e
matrixSymmetry symmetry matrix = ixmap (bounds matrix) (symmetry center) matrix
  where 
    ((begX, begY), (endX, endY)) = bounds matrix
    center = (div (begX + endX) 2, div (begY + endY) 2, even $ endY - begY + 1)

horizontalMatrixSymmetry :: (Ix i, Integral i) => MatrixSymmetry i e
horizontalMatrixSymmetry = matrixSymmetry horizontalSymmetry

verticalMatrixSymmetry :: (Ix i, Integral i) => MatrixSymmetry i e
verticalMatrixSymmetry = matrixSymmetry verticalSymmetry

rotate90Matrix :: (Ix i, Integral i) => MatrixSymmetry i e
rotate90Matrix = matrixSymmetry rotate270Symmetry

rotate270Matrix :: (Ix i, Integral i) => MatrixSymmetry i e
rotate270Matrix = matrixSymmetry rotate90Symmetry

subarray :: (Ix i) => (i, i) -> Array i e -> Array i e
subarray = \newBounds -> ixmap newBounds id

insertSubarray :: (Ix i) => Array i e -> Array i e -> Array i e
insertSubarray subarray mainArray = mainArray // (assocs subarray)
----- Data.Matrix

----- Data.Pentago
data Position = Empty | Black | White
  deriving (Eq, Ord, Show)

data Quadrant = RightTop | LeftTop | LeftBottom | RightBottom
  deriving (Eq, Ord, Show)

data RotationDirection = LeftRotation | RightRotation
  deriving (Eq, Ord, Show)

type Board = Array (Int, Int) Position

type MoveOrder = ((Int, Int), (Quadrant, RotationDirection))

data Result = BlackWin | Draw | WhiteWin
  deriving (Eq, Ord, Show)

quadrantToBounds :: Quadrant -> ((Int, Int), (Int, Int))
quadrantToBounds RightTop = ((3, 0), (5, 2))
quadrantToBounds LeftTop = ((0, 0), (2, 2))
quadrantToBounds LeftBottom = ((0, 2), (3, 5))
quadrantToBounds RightBottom = ((3, 3), (5, 5))

rotationDirectionToMatrixSymmetry :: (Ix i, Integral i) =>
  RotationDirection -> MatrixSymmetry i e
rotationDirectionToMatrixSymmetry LeftRotation = rotate90Matrix
rotationDirectionToMatrixSymmetry RightRotation = rotate270Matrix

emptyBoard :: Board
emptyBoard = array bounds [(i, Empty) | i <- range bounds]
  where
    bounds = ((0,0), (5, 5))

count :: Position -> Board -> Int
count which board = length $ filter ((== which) . snd) (assocs board)

rotateBoard :: Quadrant -> RotationDirection -> Board -> Board
rotateBoard quadrant rotationDirection board =
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

makeMove :: MoveOrder -> Board -> Board
makeMove (pos, (q, r)) = rotateBoard q r . placeToken pos

hasTheListSameNonEmptyPositions :: [Position] -> Bool
hasTheListSameNonEmptyPositions = isJust . getTheListSameNonEmptyPositions

getTheListSameNonEmptyPositions :: [Position] -> Maybe Position
getTheListSameNonEmptyPositions [] = Nothing
getTheListSameNonEmptyPositions xs = 
  if all id $ map ((&&) <$> (== (head xs)) <*> (/= Empty)) (tail xs)
  then Just $ head xs
  else Nothing

hasTheList5SameNonEmptyPositions :: [Position] -> Bool
hasTheList5SameNonEmptyPositions = isJust . getTheList5SameNonEmptyPositions

getTheList5SameNonEmptyPositions :: [Position] -> Maybe Position
getTheList5SameNonEmptyPositions [] = Nothing
getTheList5SameNonEmptyPositions (x:xs) =
  if length first5 < 5
  then Nothing
  else getTheListSameNonEmptyPositions first5
    `mplus` getTheList5SameNonEmptyPositions xs
  where first5 = take 5 (x:xs)

rowToList row = map snd (assocs row)

has5InARow :: Board -> Bool
has5InARow = isJust . get5InARow

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

getResult :: Board -> Maybe Position
getResult board = foldl' mplus Nothing [get5InARow board,
  get5InARow (rotate90Matrix board),
  get5Across board,
  get5Across (rotate90Matrix board)]
    
     
exampleGame =
  makeMove ((0,0), (RightTop, RightRotation))
  . makeMove ((0,1), (RightTop, RightRotation))
  . makeMove ((1,1), (RightTop, RightRotation))
  . makeMove ((0,2), (RightTop, RightRotation))
  . makeMove ((2,2), (RightTop, RightRotation))
  . makeMove ((0,3), (RightTop, RightRotation))
  . makeMove ((3,3), (RightTop, RightRotation))
  . makeMove ((0,4), (RightTop, RightRotation))
  . makeMove ((4,4), (RightTop, RightRotation))

allRotationOrders = do
  x <- [RightTop, LeftTop, LeftBottom, RightBottom]
  y <- [LeftRotation, RightRotation]
  return (x,y)

generatePossibleMoveOrders :: Board -> [MoveOrder]
generatePossibleMoveOrders board = do
  pos <- map fst $ filter ((== Empty) . snd) (assocs board)
  rot <- allRotationOrders
  return (pos, rot)

data PentagoHistoryState = PentagoHistoryState {
  board :: Board,
  lastMove :: MoveOrder
} deriving (Show)

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

maximize :: (RandomGen g) => g -> PentagoGameTree -> (Score, Maybe MoveOrder, g)
minimize :: (RandomGen g) => g -> PentagoGameTree -> (Score, Maybe MoveOrder, g)

maximize g board = maximize' g (-1.0) 1.0 board Nothing
minimize g board = minimize' g (-1.0) 1.0 board Nothing

maximize' :: (RandomGen g) => g
  -> Score
  -> Score
  -> PentagoGameTree
  -> Maybe (Score, MoveOrder)
  -> (Score, Maybe MoveOrder, g)

minimize' :: (RandomGen g) => g
  -> Score
  -> Score
  -> PentagoGameTree
  -> Maybe (Score, MoveOrder)
  -> (Score, Maybe MoveOrder, g)

maximize' g alpha beta (Node board []) Nothing =
  (score, Nothing, newG)
  where (score, newG) = evaluate g board

maximize' g alpha beta (Node _ []) (Just (score, moveOrder)) =
  (score, Just moveOrder, g)

maximize' g alpha beta (Node board ((childTree, moveOrder):xs)) acc =
  if score >= beta
  then (score, Just moveOrder, newG)
  else maximize' newG newAlpha beta (Node board xs) newAcc
  where
    (score, _, newG) = minimize' g alpha beta childTree Nothing
    newAcc = case acc of
      Nothing -> Just (score, moveOrder)
      Just (accScore, _) -> if score > accScore
        then Just (score, moveOrder)
        else acc
    newAlpha = max alpha score

minimize' g alpha beta (Node board []) Nothing =
  (score, Nothing, newG)
  where (score, newG) = evaluate g board

minimize' g alpha beta (Node _ []) (Just (score, moveOrder)) =
  (score, Just moveOrder, g)

minimize' g alpha beta (Node board ((childTree, moveOrder):xs)) acc =
  if score <= alpha
  then (score, Just moveOrder, newG)
  else minimize' newG alpha newBeta (Node board xs) newAcc
  where
    (score, _, newG) = maximize' g alpha beta childTree Nothing
    newAcc = case acc of
      Nothing -> Just (score, moveOrder)
      Just (accScore, _) -> if score > accScore
        then Just (score, moveOrder)
        else acc
    newBeta = min beta score

evaluate :: (RandomGen g) => g -> Board -> (Score, g)
evaluate g board = case getResult board of
  Nothing -> (0.0, g)
  Just White -> (1.0, g)
  Just Black -> (-1.0, g)

main = return () :: IO ()
