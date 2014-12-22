module Pentago.AI.Pentago where

import Pentago.AI.MinMax
import Pentago.Data.Matrix
import Pentago.Data.Pentago hiding (Player)
import Pentago.Data.Tree

import Control.Applicative
import Control.Monad.State
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Data.Traversable
import System.Random
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

newtype BoundedFloat = BoundedFloat Float
  deriving (Show, Eq, Ord)

fromFloat :: Float -> BoundedFloat
fromFloat f = if abs f > 1.0 then undefined else BoundedFloat f

instance Bounded BoundedFloat where
  maxBound = fromFloat 1.0
  minBound = fromFloat (-1.0)

type Score = BoundedFloat

type PentagoEvaluationTree = LeafValueTree MoveOrder Score

evaluateTree :: (Applicative f) => (Board -> f Score)
  -> PentagoGameTree
  -> f PentagoEvaluationTree
evaluateTree evaluateF gameTree = traverse evaluateF leafTree
  where
    leafTree = toLeafValueTree gameTree

type BoardEvaluation f = Board -> f Score

trivialEvaluate :: (Applicative f) => BoardEvaluation f
trivialEvaluate board = case getResult board of
  Nothing -> pure $ fromFloat 0.0
  Just Draw -> pure $ fromFloat (0.0)
  Just WhiteWin -> pure $ fromFloat 1.0
  Just BlackWin -> pure $ fromFloat (-1.0)

blackEvaluate :: (Applicative f) => BoardEvaluation f
blackEvaluate board = pure $ fromFloat (-1.0)

randomPlayEvaluate :: (RandomGen g) => BoardEvaluation (State g)
randomPlayEvaluate board = do
  let gameCount = 10
  plays <- Control.Monad.State.forM [1..gameCount] (\_ -> randomPlay board)
  let (whiteWins, blackWins) = Data.List.foldl'
       (\acc (_, result) -> case result of
         WhiteWin -> swap ((+ 1) <$>  swap acc)
         BlackWin -> (+ 1) <$>  acc
         Draw -> acc)
       (0,0)
       plays
  return . fromFloat $ (whiteWins - blackWins) / gameCount

randomElement :: (RandomGen g) => [a] -> State g a
randomElement list = do
  let n = length list
  gen <- get
  let (idx, newGen) = randomR (0, n-1) gen
  put newGen
  return $ list !! idx

randomPlay :: (RandomGen g) => Board -> State g (Board, Result)
randomPlay board = case getResult board of
  Nothing -> do
    let possibleMoveOrders = generatePossibleMoveOrders board
    moveOrder <- randomElement possibleMoveOrders
    randomPlay $ makeMove moveOrder board
  Just result -> return (board, result)

type Player m = Board -> m Board

type HumanPlayer = Pentago.AI.Pentago.Player IO

type RandomAIPlayer g = Pentago.AI.Pentago.Player (State g)

aiEvaluate :: (RandomGen g) => Int -> Board -> State g PentagoEvaluationTree
aiEvaluate depth board = evaluateTree randomPlayEvaluate
  . prune depth
  . generatePentagoGameTree $ board

aiPlay :: (RandomGen g) => RandomAIPlayer g
aiPlay board = 
  let possibleMovesCount = length $ generatePossiblePlacementOrders board
      depth = if possibleMovesCount > 15
              then 1
              else if possibleMovesCount > 5
              then 2
              else 3
      minMaxFunction = if fromJust (whoseTurn board) == BlackPlayer
                       then minimize
                       else maximize
  in  do
    (v, maybeMove) <- minMaxFunction <$> aiEvaluate depth board
    return $ makeMove (fromJust maybeMove) board
