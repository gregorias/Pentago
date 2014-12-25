{-|
Module : Pentago.Data.Matrix
Description :  Basic square matrix/array operations

Basic square matrix/array operations
-}
module Pentago.Data.Matrix(
  MatrixSymmetry,
  rotate90Matrix,
  rotate270Matrix,
  subarray,
  insertSubarray) where

import Data.Array.IArray
import Data.Tuple

-- Function type containing symmetry operations on array indexes.
type Symmetry i = (i, i, Bool) -- ^ YOLO
                  -> (i, i) -- ^ASD
                  -> (i, i) -- ^DS

-- s
horizontalSymmetry :: (Integral i) => Symmetry i
horizontalSymmetry (_, cY, True) = fmap (2 * cY + 1 -)
horizontalSymmetry (_, cY, _) = fmap (2 * cY -)

-- sr
transposeSymmetry :: (Integral i) => Symmetry i
transposeSymmetry (cX, cY, _) (x, y) = (cX + (y - cY), cY + (x - cX))

-- sr^2
verticalSymmetry :: (Integral i) => Symmetry i
verticalSymmetry (cX, _, True) = fmap (2 * cX + 1 -)
verticalSymmetry (cX, _, _) = fmap (2 * cX -)

-- r^2
rotate180Symmetry :: (Integral i) => Symmetry i
rotate180Symmetry center = horizontalSymmetry center . verticalSymmetry center

-- r
rotate90Symmetry :: (Integral i) => Symmetry i
rotate90Symmetry center = horizontalSymmetry center . transposeSymmetry center

-- r^3
rotate270Symmetry :: (Integral i) => Symmetry i
rotate270Symmetry center = transposeSymmetry center . horizontalSymmetry center

-- TODO quickcheck that transpose . transpose = id

-- |Group symmetry operations on square matrix
type MatrixSymmetry a i e = a (i, i) e -> a (i,i) e
-- TODO Rank2Types

matrixSymmetry :: (Ix i, Integral i, IArray a e)
  => Symmetry i -> MatrixSymmetry a i e
matrixSymmetry symmetry matrix = ixmap (bounds matrix) (symmetry center) matrix
  where 
    ((begX, begY), (endX, endY)) = bounds matrix
    center = (div (begX + endX) 2, div (begY + endY) 2, even $ endY - begY + 1)

-- |Perform OY symmetry on a matrix
horizontalMatrixSymmetry :: (Ix i, Integral i, IArray a e)
  => MatrixSymmetry a i e
horizontalMatrixSymmetry = matrixSymmetry horizontalSymmetry

-- |Perform OX symmetry on a matrix
verticalMatrixSymmetry :: (Ix i, Integral i, IArray a e)
  => MatrixSymmetry a i e
verticalMatrixSymmetry = matrixSymmetry verticalSymmetry

-- |Perform left rotation on a matrix
rotate90Matrix :: (Ix i, Integral i, IArray a e) => MatrixSymmetry a i e
rotate90Matrix = matrixSymmetry rotate270Symmetry

-- |Perform right rotation on a matrix
rotate270Matrix :: (Ix i, Integral i, IArray a e) => MatrixSymmetry a i e
rotate270Matrix = matrixSymmetry rotate90Symmetry

-- |Get subarray bounded by indexes
subarray :: (Ix i, IArray a e) => (i, i) -> a i e -> a i e
subarray = \newBounds -> ixmap newBounds id

-- |Insert subarray into array
insertSubarray :: (Ix i, IArray a e) => a i e -> a i e -> a i e
insertSubarray subarray mainArray = mainArray // (assocs subarray)
