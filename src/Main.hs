module Main(
  module Main,
  module Pentago.Data.Matrix,
  module Pentago.Data.Pentago,
  module Pentago.Data.Tree,
  module Pentago.AI.MinMax,
  ) where

import Pentago.Data.Matrix
import Pentago.Data.Pentago hiding (Player)
import Pentago.Data.Tree
import Pentago.AI.MinMax
import qualified Pentago.AI.Pentago as AP

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

main = runStateT runGame
  (SessionState initialGameState
    (mkStdGen 10)
    (Player (aiPlayerWrapper AP.randomAIPlayer) "AI 0")
    (Player (aiPlayerWrapper AP.randomAIPlayer) "AI 1"))

whichMinMax state =
  case whoseTurn state of
    Just WhitePlayer -> maximize
    Just BlackPlayer -> minimize

moveHelp = "Provide move order of form (posX, posY) (quadrant, rotation), "
  ++ "where pos in [0,5], quadrant in {RT, LT, LB, RB}, rotation in {L,R}]"

data Player = Player {
  playerWrapper :: PlayerWrapper,
  name :: String
}
  
data SessionState = SessionState {
  gameState :: GameState,
  randomGen :: StdGen,
  curPlayer :: Player,
  nextPlayer :: Player
}

runGame :: StateT SessionState IO ()
runGame = do
  sessionState <- get
  let curGameState = gameState sessionState
  liftIO . putStr . prettyShowBoard . board $ curGameState
  if isFinished curGameState
  then do
    liftIO . putStrLn
      $ (show . name $ nextPlayer sessionState) ++ " has won!"
  else do
    let curPlayerWrapper = playerWrapper . curPlayer $ sessionState
    (newGameState, newPlayerState) <- liftIO
      . runStateT (curPlayerWrapper curGameState) 
      $ (randomGen sessionState)
    put $ SessionState
      newGameState
      (newPlayerState)
      (nextPlayer sessionState)
      (curPlayer sessionState)
    runGame

type PlayerWrapperMonad = StateT StdGen IO

type PlayerWrapper = AP.Player PlayerWrapperMonad

aiPlayerWrapper :: AP.AIPlayer StdGen -> PlayerWrapper
aiPlayerWrapper aiPlayer =
  \board -> do
    gen <- get
    let (newState, newGen) = runState (aiPlayer board) gen
    put newGen
    return newState

humanPlayer :: AP.HumanPlayer
humanPlayer state = do
  putStrLn $ moveHelp
  moveOrder <- readMoveOrder
  return $ makeMove moveOrder state

humanPlayerWrapper :: PlayerWrapper
humanPlayerWrapper = lift . humanPlayer

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
