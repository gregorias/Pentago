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
  (result, _) <- runStateT
    playWithAI
    (SessionState initialGameState (mkStdGen 1) (AI "0") (AI "1"))
  putStrLn . show $ result

data TurnOrder = Player | AI String deriving (Show)

isAI :: TurnOrder -> Bool
isAI (AI _) = True
isAI _ = False
  
data SessionState = SessionState {
  gameState :: GameState,
  randomGen :: StdGen,
  curPlayer :: TurnOrder,
  nextPlayer :: TurnOrder
} deriving Show

whichMinMax state =
  case whoseTurn state of
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

playWithAI :: StateT SessionState IO Result
playWithAI = do
  state <- get
  let curGameState = gameState state
  liftIO . putStr . prettyShowBoard . board $ curGameState
  if isFinished curGameState
  then do
    liftIO . putStrLn $ (show $ nextPlayer state) ++ "has won!"
    return . fromJust . getResult $ curGameState
  else if isAI $ curPlayer state
  then do
    let (nextGameState, newState) =
          runState (randomAIPlayer (curGameState)) (randomGen state)
    put $ SessionState nextGameState (newState) (nextPlayer state)
      (curPlayer state)
    playWithAI
  else do
    nextGameState <- liftIO . humanPlay $ curGameState
    put $ SessionState nextGameState
      (randomGen state) (nextPlayer state) (curPlayer state)
    playWithAI

humanPlay :: HumanPlayer
humanPlay state = do
  putStrLn $ moveHelp
  moveOrder <- readMoveOrder
  return $ makeMove moveOrder state
