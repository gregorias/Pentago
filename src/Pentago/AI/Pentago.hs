module Pentago.AI.Pentago(
  Player,
  HumanPlayer,
  RandomAIPlayer,
  aiPlay
) where

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

type PentagoGameTree = EdgeTree MoveOrder GameState

generatePentagoGameTree :: GameState -> PentagoGameTree
generatePentagoGameTree state
  | isFinished state = ValueNode state []
  | otherwise = ValueNode state (map (fmap generatePentagoGameTree)
    uniqueChildStatesWithMoves)
  where 
    possibleMoveOrders = generatePossibleMoveOrders state
    childStatesWithMoves = map
      (\moveOrder -> (moveOrder, makeMove moveOrder state))
      possibleMoveOrders
    uniqueChildStatesWithMoves = nubBy ((==) `on` snd)
      . sortBy (compare `on` snd)
      $ childStatesWithMoves

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

evaluateTree :: (Applicative f) => (GameState -> f Score)
  -> PentagoGameTree
  -> f PentagoEvaluationTree
evaluateTree evaluateF gameTree = traverse evaluateF leafTree
  where
    leafTree = toLeafValueTree gameTree

type GameStateEvaluation f = GameState -> f Score

trivialEvaluate :: (Applicative f) => GameStateEvaluation f
trivialEvaluate state = case getResult state of
  Nothing -> pure $ fromFloat 0.0
  Just Draw -> pure $ fromFloat (0.0)
  Just WhiteWin -> pure $ fromFloat 1.0
  Just BlackWin -> pure $ fromFloat (-1.0)

blackEvaluate :: (Applicative f) => GameStateEvaluation f
blackEvaluate state = pure $ fromFloat (-1.0)

randomPlayEvaluate :: (RandomGen g) => GameStateEvaluation (State g)
randomPlayEvaluate state = do
  let gameCount = 10
  plays <- Control.Monad.State.forM [1..gameCount] (\_ -> randomPlay state)
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

randomPlay :: (RandomGen g) => GameState -> State g (GameState, Result)
randomPlay state = case getResult state of
  Nothing -> do
    let possibleMoveOrders = generatePossibleMoveOrders state
    moveOrder <- randomElement possibleMoveOrders
    randomPlay $ makeMove moveOrder state
  Just result -> return (state, result)

type Player m = GameState -> m GameState

type HumanPlayer = Pentago.AI.Pentago.Player IO

type RandomAIPlayer g = Pentago.AI.Pentago.Player (State g)

aiEvaluate :: (RandomGen g) => Int
  -> GameState
  -> State g PentagoEvaluationTree
aiEvaluate depth state = evaluateTree randomPlayEvaluate
  . prune depth
  . generatePentagoGameTree $ state

aiPlay :: (RandomGen g) => RandomAIPlayer g
aiPlay state = 
  let possibleMovesCount = length $ generatePossiblePlacementOrders state
      depth = if possibleMovesCount > 15
              then 1
              else if possibleMovesCount > 5
              then 2
              else 3
      minMaxFunction = if fromJust (whoseTurn state) == BlackPlayer
                       then minimize
                       else maximize
  in  do
    (v, maybeMove) <- minMaxFunction <$> aiEvaluate depth state
    return $ makeMove (fromJust maybeMove) state
