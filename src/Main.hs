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

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Data.Array.Unboxed
import Data.Char
import Data.Ix
import Data.List
import Data.Maybe
import Data.Tuple
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Random

faultyGame = 
  makeMove ((5,1), (LeftTop, RightRotation))
  . makeMove ((5,2), (RightBottom, LeftRotation))
  . makeMove ((5,0), (RightBottom, RightRotation))
  . makeMove ((4,2), (RightBottom, LeftRotation))
  . makeMove ((4,0), (RightBottom, RightRotation))
  . makeMove ((5,3), (RightTop, LeftRotation))
  . makeMove ((4,1), (RightTop, RightRotation))
  . makeMove ((5,4), (RightTop, LeftRotation))
  . makeMove ((3,2), (RightTop, RightRotation))
  . makeMove ((5,5), (RightTop, RightRotation))
  . makeMove ((2,5), (RightTop, RightRotation))
  . makeMove ((4,5), (RightTop, RightRotation))
  . makeMove ((2,4), (RightTop, RightRotation))
  . makeMove ((3,5), (RightTop, RightRotation))
  . makeMove ((1,4), (RightTop, RightRotation))
  . makeMove ((2,3), (RightTop, RightRotation))
  . makeMove ((1,3), (RightTop, RightRotation))
  . makeMove ((2,2), (RightTop, RightRotation))
  . makeMove ((2,1), (RightTop, RightRotation))
  . makeMove ((1,1), (RightTop, RightRotation))
  $ initialUnboxedGameState
{-------------------------
| . | . | . | . | x | x |
-------------------------
| . | o | . | . | x | x |
-------------------------
| o | x | . | x | o | o |
-------------------------
| . | x | o | . | . | o |
-------------------------
| . | x | x | . | . | o |
-------------------------
| . | . | x | o | o | o |
-------------------------}
--
{-------------------------
| . | . | . | x | . | . |
-------------------------
| . | o | . | o | x | x |
-------------------------
| o | x | . | o | x | x |
-------------------------
| . | x | o | o | . | o |
-------------------------
| . | x | x | . | . | o |
-------------------------
| . | . | x | o | o | o |
-------------------------}

----- Data.Pentago
    
{- main = runStateT mainMenu
  (MainMenuState 
    (Player (aiPlayerWrapper AP.randomAIPlayer) "AI 0")
    (Player (aiPlayerWrapper AP.randomAIPlayer) "AI 1")) -}

main = trialGame

trialGame = runStateT runGame
      $ SessionState initialUnboxedGameState (mkStdGen 0)
      (Player (aiPlayerWrapper AP.randomAIPlayer) "AI 0")
      (Player (aiPlayerWrapper AP.randomAIPlayer) "AI 1")

-- main menu
type ChosenGameStateType = UnboxedGameState

data MainMenuState = MainMenuState {
  firstPlayer :: Player ChosenGameStateType,
  secondPlayer :: Player ChosenGameStateType
}

mainMenuString =
  "1) Start game" ++ "\n"
  ++ "2) Configure" ++ "\n"
  ++ "3) Exit" ++ "\n"
 
mainMenu :: StateT MainMenuState IO ()
mainMenu = do
  liftIO $ putStr mainMenuString
  option <- head <$> liftIO getLine
  liftIO $ putStrLn ""
  if option == '1'
  then do
    curPlayer <- firstPlayer <$> get
    nextPlayer <- secondPlayer <$> get
    lift $ do
      stdGen <- newStdGen
      runStateT runGame
        $ SessionState initialUnboxedGameState stdGen curPlayer nextPlayer
    mainMenu
  else if option == '2'
  then do
    configurationMenu
    mainMenu
  else
    return ()


-- configuration menu
switchPlayer :: (GameState s) => Player s -> Player s
switchPlayer player = 
  if playerName == "Human"
  then Player (aiPlayerWrapper AP.randomAIPlayer) ("AI " ++ idx)
  else Player (humanPlayerWrapper) ("Human " ++ idx)
  where (playerName:(idx:[])) = words $ name player

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

configurationMenu :: StateT MainMenuState IO ()
configurationMenu = do
  mainMenuState <- get
  let curFirstPlayer = firstPlayer mainMenuState
      curSecondPlayer = secondPlayer mainMenuState
  which <- lift $ do 
    showCurrentState mainMenuState
    putStrLn ""
    option <- configurationMenuMainLoop
    putStrLn ""
    if option == '1'
    then return 1
    else if option == '2'
    then return 2
    else return 3
  if which == 1
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
  playerWrapper :: PlayerWrapper s,
  name :: String
}
  
data SessionState = SessionState {
  gameState :: ChosenGameStateType,
  randomGen :: StdGen,
  curPlayer :: Player ChosenGameStateType,
  nextPlayer :: Player ChosenGameStateType
}

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
    in do
      liftIO . putStrLn $ winMessage
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

type PlayerWrapper s = AP.Player PlayerWrapperMonad s

aiPlayerWrapper :: (GameState s) => AP.AIPlayer s StdGen -> PlayerWrapper s
aiPlayerWrapper aiPlayer =
  \board -> do
    gen <- get
    let (newState, newGen) = runState (aiPlayer board) gen
    put newGen
    return newState

humanPlayer :: (GameState s) => AP.HumanPlayer s
humanPlayer state = do
  putStrLn $ moveHelp
  moveOrder <- readMoveOrder
  return $ makeMove moveOrder state

humanPlayerWrapper :: (GameState s) => PlayerWrapper s
humanPlayerWrapper = lift . humanPlayer

whichMinMax state =
  case whoseTurn state of
    Just WhitePlayer -> maximize
    Just BlackPlayer -> minimize

moveHelp = "Provide move order of form posX posY quadrant rotation, "
  ++ "where pos in [0,5], quadrant in {RT, LT, LB, RB}, rotation in {L,R}]"
-- main Menu

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
