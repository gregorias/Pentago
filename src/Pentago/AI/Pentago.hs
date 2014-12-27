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
import Pentago.Data.Pentago hiding (Player)
import Pentago.Data.Tree

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array (array)
import Data.Array.Base
import Data.Array.ST
import Data.List
import Data.Maybe
import Data.Tuple
import System.Random

import Data.STRef

type PentagoGameTree s = EdgeTree MoveOrder s

-- | Generate complete game tree from current state to all possible states
generatePentagoGameTree :: (GameState s) => s -> PentagoGameTree s
generatePentagoGameTree gameState
  | isFinished gameState = ValueNode gameState []
  | otherwise = ValueNode gameState (map (fmap generatePentagoGameTree)
    childStatesWithMoves)
  where 
    possibleMoveOrders = getPossibleMoveOrders gameState
    childStatesWithMoves = map
      (\moveOrder -> (moveOrder, makeMove moveOrder gameState))
      possibleMoveOrders

prune :: Int -> PentagoGameTree s -> PentagoGameTree s
prune 0 (ValueNode a _) = ValueNode a []
prune d (ValueNode a xs) = ValueNode a $ map (fmap $ prune (d - 1)) xs

randomElement :: (RandomGen g) => [a] -> State g a
randomElement list = do
  let n = length list
  gen <- get
  let (idx, newGen) = randomR (0, n-1) gen
  put newGen
  return $ list !! idx

-- Copied from https://www.haskell.org/haskellwiki/Random_shuffle
-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: (RandomGen g) => [a] -> g -> ([a], g)
shuffle' xs gen = runST $ do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray' n xs
        xs' <- Control.Monad.forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen')
  where
    n = length xs
    newArray' :: Int -> [a] -> ST s (STArray s Int a)
    newArray' m ys =  newListArray (1, m) ys

forceList :: [a] -> Int
forceList xs = forceList' xs 0

forceList' :: [a] -> Int -> Int
forceList' [] n = n
forceList' (x:xs) n = x `seq` forceList' xs (n + 1)

-- | Randomly shuffle a list
shuffle :: (RandomGen g) => [a] -> State g [a]
shuffle [] = return []
shuffle xs =
  let n = length xs
      indexes = [0..(n - 1)]
      xsArray = Data.Array.array (0, (n - 1)) (zip indexes xs)
  in do
    gen <- get
    let
      (orderList, newGen) = shuffle' indexes gen
      newXs = map (\i -> xsArray ! i) orderList
    put newGen
    return $ forceList newXs `seq` newXs

shuffleFirstChildrenInATree :: (RandomGen g)
  => LeafValueTree e v
  -> State g (LeafValueTree e v)
shuffleFirstChildrenInATree tree@(Leaf _) = return tree
shuffleFirstChildrenInATree (Node xs) = do
  newXs <- shuffle xs
  return $ (length newXs) `seq` Node newXs

newtype BoundedFloat = BoundedFloat Float
  deriving (Show, Eq, Ord)

fromFloat :: Float -> BoundedFloat
fromFloat f = if abs f > 1.0 then undefined else BoundedFloat f

instance Bounded BoundedFloat where
  maxBound = fromFloat 1.0
  minBound = fromFloat (-1.0)

type Score = BoundedFloat

type PentagoEvaluationTree = LeafValueTree MoveOrder Score

evaluateTree :: (s -> Score) -> LeafValueTree MoveOrder s -> PentagoEvaluationTree
evaluateTree evaluateF = fmap evaluateF

-- |Appends to each value in LeafValueTree an indpendent random generator.
splitRandomGenOverTree :: (RandomGen g)
  => g
  -> LeafValueTree e s
  -> LeafValueTree e (s, g)
splitRandomGenOverTree g (Leaf s) = Leaf (s, g)
splitRandomGenOverTree g (Node xs) = Node $
  fmap fst . tail $ scanl scanF (undefined, g) xs
  where 
    scanF (_, g') (e, t) = ((e, splitRandomGenOverTree g0 t), g1)
      where (g0, g1) = split g'

type GameStateEvaluation s = s -> Score

trivialEvaluate :: (GameState s) => GameStateEvaluation s
trivialEvaluate gameState = case getResult gameState of
  Nothing -> fromFloat 0.0
  Just Draw -> fromFloat 0.0
  Just WhiteWin -> fromFloat 1.0
  Just BlackWin -> fromFloat (-1.0)

randomPlayEvaluate :: (GameState s, RandomGen g)
  => GameStateEvaluation (s, g)
randomPlayEvaluate (gameState, gen) = fst $ runState (do
    let gameCount = 2
    plays <- Control.Monad.State.forM
      [1..gameCount]
      (\_ -> randomPlay gameState)
    let (whiteWins, blackWins) = Data.List.foldl'
         (\acc (_, result) -> case result of
           WhiteWin -> swap ((+ 1) <$>  swap acc)
           BlackWin -> (+ 1) <$>  acc
           Draw -> acc)
         (0,0)
         plays
    return . fromFloat $ (whiteWins - blackWins) / gameCount)
  gen

randomPlay :: (GameState s, RandomGen g)
  => s
  -> State g (s, Result)
randomPlay gameState = case getResult gameState of
  Nothing -> do
    let possibleMoveOrders = getPossibleMoveOrders gameState
    moveOrder <- randomElement possibleMoveOrders
    randomPlay $ makeMove moveOrder gameState
  Just result -> return (gameState, result)

-- | Pentago player is a function from current game state to monadic evaluation
-- returning next game state
type Player m s = s -> m s

type HumanPlayer s = Pentago.AI.Pentago.Player IO s

type AIPlayer s g = Pentago.AI.Pentago.Player (State g) s

randomAIPlayer :: (GameState s, RandomGen g) => AIPlayer s g
randomAIPlayer gameState = 
  let possibleMovesCount = length $ getPossiblePlacementOrders gameState
      depth = if possibleMovesCount > 20
              then 1
              else if possibleMovesCount > 5
              then 2
              else 3
      minMaxFunction = if fromJust (whoseTurn gameState) == BlackPlayer
                       then minimize
                       else maximize
      prunedLeafValueTree = toLeafValueTree
        . prune depth
        $ generatePentagoGameTree gameState 
  in  do
    shuffledTree <- shuffleFirstChildrenInATree prunedLeafValueTree
    gen <- get
    let (gen0, gen1) = split gen
        shuffledSplitTree = splitRandomGenOverTree gen0 shuffledTree
        (_, maybeMove) = minMaxFunction 
                           $ evaluateTree randomPlayEvaluate shuffledSplitTree
    put gen1
    return $ makeMove (fromJust maybeMove) gameState

trivialAIPlayer :: (GameState s, RandomGen g) => Int -> AIPlayer s g
trivialAIPlayer initialDepth gameState =
  let possibleMovesCount = length $ getPossiblePlacementOrders gameState
      depth = if possibleMovesCount > 10
              then initialDepth
              else initialDepth + 1
      minMaxFunction = if fromJust (whoseTurn gameState) == BlackPlayer
                       then minimize
                       else maximize
      prunedLeafValueTree = toLeafValueTree
        . prune depth
        $ generatePentagoGameTree gameState 
  in  do
    shuffledTree <- shuffleFirstChildrenInATree prunedLeafValueTree
    let (_, maybeMove) = minMaxFunction
                           $ evaluateTree trivialEvaluate shuffledTree
    return $ makeMove (fromJust maybeMove) gameState
