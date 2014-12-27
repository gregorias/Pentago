{-
Module : Main
Description : UI and top level game loop

Module which handles UI and top level game loop.
-}
module Main(
  main,
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

import Control.Applicative
import Control.Monad.State
import Data.Char
import Text.ParserCombinators.Parsec
import System.Random

type GameStateType = SmartGameState

initialGameState :: GameStateType
initialGameState = initialSmartGameState
    
main :: IO () 
main = fst <$> runStateT mainMenu
  (MainMenuState 
    (Player humanPlayerWrapper "Human 0")
    (Player (aiPlayerWrapper $ AP.trivialAIPlayer 3) "AI 1"))

-- main = trialGame

-- |IO Monad which runs a game between two AI players.
{- trialGame = runStateT runGame
      $ SessionState initialGameState (mkStdGen 0)
      (Player (aiPlayerWrapper $ AP.trivialAIPlayer 3) "AI 0")
      (Player (aiPlayerWrapper $ AP.trivialAIPlayer 3) "AI 1") -}

-- main menu
data MainMenuState = MainMenuState {
  firstPlayer :: Player GameStateType,
  secondPlayer :: Player GameStateType
}

mainMenuString :: String
mainMenuString =
  "1) Start game" ++ "\n"
  ++ "2) Configure" ++ "\n"
  ++ "3) Exit" ++ "\n"
 
mainMenu :: StateT MainMenuState IO ()
mainMenu = do
  liftIO $ putStr mainMenuString
  menuOption <- head <$> liftIO getLine
  liftIO $ putStrLn ""
  if menuOption == '1'
  then do
    firstPlayer' <- firstPlayer <$> get
    secondPlayer' <- secondPlayer <$> get
    lift $ do
      stdGen <- newStdGen
      _ <- runStateT runGame
        $ SessionState initialGameState stdGen firstPlayer' secondPlayer'
      return ()
    mainMenu
  else if menuOption == '2'
  then do
    configurationMenu
    mainMenu
  else
    return ()

-- configuration menu
switchPlayer :: (GameState s) => Player s -> Player s
switchPlayer player = 
  if playerName == "Human"
  then Player (aiPlayerWrapper $ AP.trivialAIPlayer 3) ("AI " ++ idx)
  else Player (humanPlayerWrapper) ("Human " ++ idx)
  where (playerName:(idx:[])) = words $ name player

configurationMenuString :: String
configurationMenuString =
  "1) Switch first player" ++ "\n"
  ++ "2) Switch second player" ++ "\n"
  ++ "3) Go to main menu" ++ "\n"

showCurrentState :: MainMenuState -> IO ()
showCurrentState mainMenuState = do
  putStrLn $ "1. player: " ++ (name . firstPlayer $ mainMenuState)
  putStrLn $ "2. player: " ++ (name . secondPlayer $ mainMenuState)

configurationMenuMainLoop :: IO Char
configurationMenuMainLoop = do
  putStr configurationMenuString
  head <$> getLine

-- |Configuration menu allowing user to choose player types.
configurationMenu :: StateT MainMenuState IO ()
configurationMenu = do
  mainMenuState <- get
  let curFirstPlayer = firstPlayer mainMenuState
      curSecondPlayer = secondPlayer mainMenuState
  which <- lift $ do 
    showCurrentState mainMenuState
    putStrLn ""
    menuOption <- configurationMenuMainLoop
    putStrLn ""
    if menuOption == '1'
    then return 1
    else if menuOption == '2'
    then return 2
    else return 3
  if which == (1 :: Int)
  then do
    put $ MainMenuState (switchPlayer curFirstPlayer) curSecondPlayer
    configurationMenu
  else if which == 2
  then do
    put $ MainMenuState curFirstPlayer (switchPlayer curSecondPlayer)
    configurationMenu
  else
    return ()

-- runGame

data Player s = Player {
  playerWrapper :: PlayerWrapper s -- ^Wrapper for player function
  , name :: String -- ^Human readable player name
}
  
data SessionState = SessionState {
  gameState :: GameStateType,
  randomGen :: StdGen,
  curPlayer :: Player GameStateType,
  nextPlayer :: Player GameStateType
}

-- |Runs a game between two players displaying current board betwen moves.
runGame :: StateT SessionState IO ()
runGame = do
  sessionState <- get
  let curGameState = gameState sessionState
  liftIO . putStr . prettyShowBoard . getBoardArray $ curGameState
  if isFinished curGameState
  then 
    let
      result = getResult curGameState
      winMessage = case result of
        Just Draw -> "The game has ended in a draw."
        Just WhiteWin -> "The white player has won."
        Just BlackWin -> "The black player has won."
        Nothing -> error "getResult has returned Nothing."
    in do
      liftIO . putStrLn $ winMessage
  else do
    let curPlayerWrapper = playerWrapper . curPlayer $ sessionState
    (newGameState, newPlayerState) <- liftIO
      . runStateT (curPlayerWrapper curGameState) 
      $ (randomGen sessionState)
    put $ SessionState
      newGameState
      newPlayerState
      (nextPlayer sessionState)
      (curPlayer sessionState)
    runGame

type PlayerWrapperMonad = StateT StdGen IO

-- |Wrapper for Pentago.AI.Pentago.Player function which unifies monads used by
-- AI and human player.
type PlayerWrapper s = AP.Player PlayerWrapperMonad s

aiPlayerWrapper :: (GameState s) => AP.AIPlayer s StdGen -> PlayerWrapper s
aiPlayerWrapper aiPlayer =
  \board -> do
    gen <- get
    let (newState, newGen) = runState (aiPlayer board) gen
    put newGen
    return newState

humanPlayer :: (GameState s) => AP.HumanPlayer s
humanPlayer currentGameState = do
  putStrLn $ moveHelp
  moveOrder <- readMoveOrder
  return $ makeMove moveOrder currentGameState

humanPlayerWrapper :: (GameState s) => PlayerWrapper s
humanPlayerWrapper = lift . humanPlayer

moveHelp :: String
moveHelp = "Provide move order of form posX posY quadrant rotation, "
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
