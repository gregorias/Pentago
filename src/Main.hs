module Main(
  module Main,
  module Pentago.Data.Matrix,
  module Pentago.Data.Pentago,
  module Pentago.Data.Tree,
  module Pentago.AI.MinMax,
  module Pentago.AI.Pentago,
  ) where

import Pentago.Data.Matrix
import Pentago.Data.Pentago
import Pentago.Data.Tree
import Pentago.AI.MinMax
import Pentago.AI.Pentago

import Control.Monad.Identity
import Data.Array
import Data.Ix
import Data.List
import Data.Maybe
import Data.Tuple
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

main = testSpeed

findBestMove depth f = f
  . runIdentity
  . evaluateTree trivialEvaluate
  . prune depth
  . generatePentagoGameTree

makeBestMove depth f board = makeMove
  (fromJust . snd $ findBestMove depth f board)
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
