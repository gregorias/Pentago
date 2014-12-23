module Pentago.Data.Matrix(
  MatrixSymmetry,
  matrixSymmetry,
  rotate90Matrix,
  rotate270Matrix,
  subarray,
  insertSubarray) where

import Data.Array
import Data.Tuple

type Symmetry i = (i, i, Bool) -> (i, i) -> (i, i)

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

type MatrixSymmetry i e = Array (i, i) e -> Array (i,i) e

matrixSymmetry :: (Ix i, Integral i) => Symmetry i -> MatrixSymmetry i e
matrixSymmetry symmetry matrix = ixmap (bounds matrix) (symmetry center) matrix
  where 
    ((begX, begY), (endX, endY)) = bounds matrix
    center = (div (begX + endX) 2, div (begY + endY) 2, even $ endY - begY + 1)

horizontalMatrixSymmetry :: (Ix i, Integral i) => MatrixSymmetry i e
horizontalMatrixSymmetry = matrixSymmetry horizontalSymmetry

verticalMatrixSymmetry :: (Ix i, Integral i) => MatrixSymmetry i e
verticalMatrixSymmetry = matrixSymmetry verticalSymmetry

rotate90Matrix :: (Ix i, Integral i) => MatrixSymmetry i e
rotate90Matrix = matrixSymmetry rotate270Symmetry

rotate270Matrix :: (Ix i, Integral i) => MatrixSymmetry i e
rotate270Matrix = matrixSymmetry rotate90Symmetry

subarray :: (Ix i) => (i, i) -> Array i e -> Array i e
subarray = \newBounds -> ixmap newBounds id

insertSubarray :: (Ix i) => Array i e -> Array i e -> Array i e
insertSubarray subarray mainArray = mainArray // (assocs subarray)
