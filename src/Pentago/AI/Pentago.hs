{-|
Module : Pentago.AI.Pentago
Description : Implementation of Pentago AI

Implementation of Pentago AI
-}
module Pentago.AI.Pentago(
  Player
  , HumanPlayer
  , AIPlayer
  , randomAIPlayer
  , trivialAIPlayer
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

type PentagoGameTree s = EdgeTree MoveOrder s

-- | Generate complete game tree from current state to all possible states
generatePentagoGameTree :: (GameState s) => s -> PentagoGameTree s
generatePentagoGameTree state
  | isFinished state = ValueNode state []
  | otherwise = ValueNode state (map (fmap generatePentagoGameTree)
    childStatesWithMoves)
  where 
    possibleMoveOrders = getPossibleMoveOrders state
    childStatesWithMoves = map
      (\moveOrder -> (moveOrder, makeMove moveOrder state))
      possibleMoveOrders
    {-uniqueChildStatesWithMoves = nubBy ((==) `on` (getBoardArray . snd))
       . sortBy (compare `on` (getBoardArray . snd))
       $ childStatesWithMoves -}

sizeOfGameTree :: PentagoGameTree s -> Int
sizeOfGameTree = Data.Foldable.foldl' (+) 0 . fmap (const 1)

prune :: Int -> PentagoGameTree s -> PentagoGameTree s
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

evaluateTree :: (GameState s, Applicative f) => (s -> f Score)
  -> PentagoGameTree s
  -> f PentagoEvaluationTree
evaluateTree evaluateF gameTree = traverse evaluateF leafTree
  where
    leafTree = toLeafValueTree gameTree

type GameStateEvaluation s f = s -> f Score

trivialEvaluate :: (GameState s, Applicative f) => GameStateEvaluation s f
trivialEvaluate state = case getResult state of
  Nothing -> pure $ fromFloat 0.0
  Just Draw -> pure $ fromFloat (0.0)
  Just WhiteWin -> pure $ fromFloat 1.0
  Just BlackWin -> pure $ fromFloat (-1.0)

blackEvaluate :: (GameState s, Applicative f) => GameStateEvaluation s f
blackEvaluate state = pure $ fromFloat (-1.0)

randomPlayEvaluate :: (GameState s, RandomGen g)
  => GameStateEvaluation s (State g)
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

randomPlay :: (GameState s, RandomGen g)
  => s
  -> State g (s, Result)
randomPlay state = case getResult state of
  Nothing -> do
    let possibleMoveOrders = getPossibleMoveOrders state
    moveOrder <- randomElement possibleMoveOrders
    randomPlay $ makeMove moveOrder state
  Just result -> return (state, result)

randomPlay' :: (GameState s, RandomGen g)
  => s
  -> State g (s, MoveOrder)
randomPlay' state = case getResult state of
  Nothing -> do
    let possibleMoveOrders = getPossibleMoveOrders state
    moveOrder <- randomElement possibleMoveOrders
    return $ (makeMove moveOrder state, moveOrder)
  Just result -> return (state, ((0,0), (RightTop, RightRotation)))

-- | Pentago player is a function from current game state to monadic evaluation
-- returning next game state
type Player m s = s -> m s

type HumanPlayer s = Pentago.AI.Pentago.Player IO s

type AIPlayer s g = Pentago.AI.Pentago.Player (State g) s

aiEvaluate :: (GameState s, RandomGen g)
  => GameStateEvaluation s (State g)
  -> Int
  -> s
  -> State g PentagoEvaluationTree
aiEvaluate stateEvaluation depth state =
  evaluateTree stateEvaluation
  . prune depth
  . generatePentagoGameTree $ state

randomAIPlayer :: (GameState s, RandomGen g) => AIPlayer s g
randomAIPlayer state = 
  let possibleMovesCount = length $ getPossiblePlacementOrders state
      depth = if possibleMovesCount > 10
              then 1
              else if possibleMovesCount > 5
              then 2
              else 3
      minMaxFunction = if fromJust (whoseTurn state) == BlackPlayer
                       then minimize
                       else maximize
  in  do
    (v, maybeMove) <- minMaxFunction
      <$> (aiEvaluate randomPlayEvaluate) depth state
    return $ makeMove (fromJust maybeMove) state

trivialAIPlayer :: (GameState s, RandomGen g) => AIPlayer s g
trivialAIPlayer state = 
  let possibleMovesCount = length $ getPossiblePlacementOrders state
      depth = if possibleMovesCount > 20
              then 2
              else if possibleMovesCount > 5
              then 3
              else 4
      minMaxFunction = if fromJust (whoseTurn state) == BlackPlayer
                       then minimize
                       else maximize
  in  do
    (v, maybeMove) <- minMaxFunction
      <$> (aiEvaluate trivialEvaluate) depth state
    return $ makeMove (fromJust maybeMove) state
