import Control.Applicative
import Data.Array
import Data.Ix
import Data.List
import Data.Tuple
import System.Random

-----
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
-----
subarray :: (Ix i) => (i, i) -> Array i e -> Array i e
subarray = \newBounds -> ixmap newBounds id

insertSubarray :: (Ix i) => Array i e -> Array i e -> Array i e
insertSubarray subarray mainArray = mainArray // (assocs subarray)
------

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
hasTheListSameNonEmptyPositions [] = False
hasTheListSameNonEmptyPositions xs = all id
  $ map ((&&) <$> (== (head xs)) <*> (/= Empty)) (tail xs)

hasTheList5SameNonEmptyPositions :: [Position] -> Bool
hasTheList5SameNonEmptyPositions [] = False
hasTheList5SameNonEmptyPositions (x:xs) =
  if length first5 < 5
  then False
  else hasTheListSameNonEmptyPositions first5
    || hasTheList5SameNonEmptyPositions xs
  where first5 = take 5 (x:xs)

rowToList row = map snd (assocs row)

has5InARow :: Board -> Bool
has5InARow board = any id $ map has5InARow'
  $ map (\i -> subarray ((0, i), (5, i)) board) [0..5]

has5InARow' row = hasTheList5SameNonEmptyPositions $ rowToList row

has5Across board = any id $
  map (hasTheList5SameNonEmptyPositions . rowToList) [rowA, rowB, rowC]
  where rowA = ixmap (0, 5) (\i -> (i, i)) board
        rowB = ixmap (0, 4) (\i -> (i + 1, i)) board
        rowC = ixmap (0, 4) (\i -> (i, i + 1)) board


isFinished :: Board -> Bool
isFinished board = has5InARow board
  || has5InARow (rotate90Matrix board)
  || has5Across board
  || has5Across (rotate90Matrix board)
    
     
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

data GameTree = Node Board [GameTree] | Leaf Board
  deriving (Show)

generateGameTree :: Board -> GameTree
generateGameTree board 
  | isFinished board = Leaf board
  | otherwise = Node board (map generateGameTree (nub . sort $ map
    (\moveOrder -> makeMove moveOrder board)
    (generatePossibleMoveOrders board)))

sizeOfGameTree :: GameTree -> Int
sizeOfGameTree (Leaf _) = 1
sizeOfGameTree (Node _ xs) = 1 + foldl'
  (\a x -> let z = sizeOfGameTree x in seq z (a + z)) 0 xs

prune :: Int -> GameTree -> GameTree
prune 0 (Node a _) = Leaf a
prune 0 a = a
prune d (Leaf a) = Leaf a
prune d (Node a xs) = Node a $ map (prune (d - 1)) xs

-- evaluateGame :: (RandomGen g) -> g -> Board -> (RandomGen, Result)
-- evaluateGame random board =
  -- let maybeResult = calculateResult board
  -- case maybeResult of
    -- Nothing -> evaluateGame 
    -- Just result -> (random, result)

--  if isFinished board then 


-- generatePositions :: Board -> [Board]
-- generatePositions board
