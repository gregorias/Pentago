module Main(
  module Main,
  module Pentago.Data.Matrix,
  module Pentago.Data.Pentago,
  module Pentago.Data.Tree,
  module Pentago.AI.MinMax,
  module Pentago.AI.Pentago,
  ) where

import Pentago.Data.Matrix
import Pentago.Data.Pentago hiding (Player)
import Pentago.Data.Tree
import Pentago.AI.MinMax
import Pentago.AI.Pentago

import Control.Monad.Identity
import Control.Monad.State
import Data.Array
import Data.Char
import Data.Ix
import Data.List
import Data.Maybe
import Data.Tuple
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Random

----- Data.Pentago
    
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

beginningGame =
  makeMove ((1,1), (RightTop, RightRotation))
  . makeMove ((4,4), (RightTop, RightRotation))
  . makeMove ((1,4), (RightTop, RightRotation))
  . makeMove ((4,1), (RightTop, RightRotation))
  . makeMove ((2,1), (RightTop, RightRotation))

main = do
  (result, _) <-
    runStateT playWithAI (GameState emptyBoard (mkStdGen 1) (AI "0") (AI "1"))
  putStrLn . show $ result

findBestMove depth f evaluate = f
  . runIdentity
  . evaluateTree evaluate
  . prune depth
  . generatePentagoGameTree

makeBestMove depth f board = makeMove
  (fromJust . snd $ findBestMove depth f trivialEvaluate board)
  board

testSpeed = putStrLn . show $ sizeOfGameTree . prune 2 $ g
  where
    e = emptyBoard
    f = exampleGame e
    f2 = f // [((4,4), Empty)]
    g = generatePentagoGameTree f2

data S = M | Z | P deriving (Show, Ord, Eq, Bounded)
testT = Node [(0, Node [(1, Leaf Z)]), (2, Node [(3, Leaf P), undefined])]

testMaximize = runIdentity $ evaluateTree trivialEvaluate . prune 1 $ g
  where
    e = emptyBoard
    f = exampleGame e
    f2 = f // [((4,4), Empty)]
    g = generatePentagoGameTree f2

data TurnOrder = Player | AI String deriving (Show)

isAI :: TurnOrder -> Bool
isAI (AI _) = True
isAI _ = False
  
data GameState = GameState {
  board :: Board,
  randomGen :: StdGen,
  curPlayer :: TurnOrder,
  nextPlayer :: TurnOrder
} deriving Show

newGameState = GameState emptyBoard (mkStdGen 0) Player (AI "0")

whichMinMax board =
  case whoseTurn board of
    Just WhitePlayer -> maximize
    Just BlackPlayer -> minimize

moveHelp = "Provide move order of form (posX, posY) (quadrant, rotation), "
  ++ "where pos in [0,5], quadrant in {RT, LT, LB, RB}, rotation in {L,R}]"

parsePosition :: Parser Int
parsePosition = do
  posX <- digit
  let diff = (ord posX) - (ord '0')
  if diff > 5
  then
    fail "Read position is too large."
  else 
    return diff

parseQuadrant :: Parser Quadrant
parseQuadrant = do
  lr <- oneOf "RL"
  tb <- oneOf "TB"
  let quadrant = [lr, tb]
  if quadrant == "RT"
  then
    return RightTop
  else if quadrant == "LT"
  then
    return LeftTop
  else if quadrant == "LB"
  then
    return LeftBottom
  else
    return RightBottom

parseRotation :: Parser RotationDirection
parseRotation = do
  lr <- oneOf "RL"
  if lr == 'R' then return RightRotation else return LeftRotation

parseMoveOrder :: Parser MoveOrder
parseMoveOrder = do
  spaces
  posX <- parsePosition
  spaces
  posY <- parsePosition
  spaces
  quadrant <- parseQuadrant
  spaces
  rotation <- parseRotation
  spaces
  return ((posX, posY), (quadrant, rotation))

readMoveOrder :: IO MoveOrder
readMoveOrder = do
  line <- getLine
  case parse parseMoveOrder "MoveOrder Parser" line of
    Left err -> putStrLn (show err) >> readMoveOrder
    Right moveOrder -> return moveOrder

playWithAI :: StateT GameState IO Result
playWithAI = do
  state <- get
  let curBoard = board state
  liftIO . putStr $ prettyShowBoard curBoard
  if isFinished curBoard
  then do
    liftIO . putStrLn $ (show $ nextPlayer state) ++ "has won!"
    return . fromJust . getResult $ curBoard
  else if isAI $ curPlayer state
  then do
    let (nextBoard, newState) = runState (aiPlay (curBoard)) (randomGen state)
    put $ GameState nextBoard (newState) (nextPlayer state) (curPlayer state)
    playWithAI
    -- let nextBoard = makeBestMove 2 (whichMinMax curBoard) curBoard
    -- put $ GameState nextBoard (nextPlayer state) (curPlayer state)
    -- playWithAI
  else do
    nextBoard <- liftIO . humanPlay $ curBoard
    put $ GameState nextBoard (randomGen state) (nextPlayer state) (curPlayer state)
    playWithAI

humanPlay :: HumanPlayer
humanPlay board = do
  putStrLn $ moveHelp
  moveOrder <- readMoveOrder
  return $ makeMove moveOrder board
