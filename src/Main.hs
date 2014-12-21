module Main where

import Pentago.Data.Matrix
import Pentago.Data.Pentago
import Pentago.AI.MinMax
import Pentago.AI.Pentago

import Data.Array
import Data.Ix
import Data.List
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

main = testSpeed

testSpeed = putStrLn . show $ sizeOfGameTree . prune 3 $ g
  where
    e = emptyBoard
    f = exampleGame e
    f2 = f // [((4,4), Empty)]
    g = generatePentagoGameTree f2
