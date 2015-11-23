module Analysis.Graph.SpectralGraphAnalysis where

import Data.Packed.Matrix (Matrix, trans, toRows, toColumns, mapMatrix, rows, (><), flatten)
import Numeric.LinearAlgebra.Algorithms (pinv)
import Numeric.LinearAlgebra.LAPACK (eigS')
import Numeric.LinearAlgebra.Util (diagl)
import Numeric.Container ((<>))
import Data.Packed.Vector (Vector, foldVector, toList)
--import Numeric.FastMath --makes no difference, all math is done by LAPACK

-- The signal flow and the graph Laplacian of the symmetrized adjacency matrix are implemented as defined in the supplemental material of the paper:
-- Varshney LR, Chen BL, Paniagua E, Hall DH, Chklovskii DB (2011) Structural Properties of the Caenorhabditis elegans Neuronal Network. PLoS Comput Biol 7(2): e1001066. doi:10.1371/journal.pcbi.1001066
--
-- Casey Schneider-Mizell kindly hand-holded me in understanding the mathematical formulae.


-- Return a list with the first element being (-1, signalFlow)
-- and the rest being (eigenvalue, eigenvector) pairs sorted by eigenvalue ascending.
-- Only includes non-zero eigenvalue entries.
run :: Matrix Double -> [(Double, Vector Double)]
run adjM = (-1, z) : e
    where
          -- transpose
          t = trans adjM
          -- symmetrized adjacency matrix
          w = (adjM + t) / 2
          -- diagonalized circuit graph degrees (number of edges per node)
          d = diagl $ map (foldVector (+) 0) $ toRows w
          -- graph Laplacian
          l = d - w
          --
          z = signalFlow' w (adjM - t) (pinv l)
          e = nonZeroSortedEigen 0.0000000001 d l


-- Compute the signal flow from:
--  * w: the symmetrized adjacency matrix,
--  * io: the difference between inputs and outputs (the adjacency matrix minus its transpose)
--  * inv': the pseudoinverse of the graph laplacian.
signalFlow' :: Matrix Double -> Matrix Double -> Matrix Double -> Vector Double
signalFlow' w io inv' = z
    where sign v
            | 0 == v     = 0
            | v < 0      = -1
            | otherwise  = 1
          signs = mapMatrix sign io
          b = (rows w) >< 1 $ map (foldVector (+) 0) $ toRows $ w * signs
          z = flatten $ inv' <> b


-- * epsilon: threshold below which a eigenvalue is considered zero
-- * d: diagonalized circuit graph degrees
-- * l: graph laplacian
nonZeroSortedEigen :: Double -> Matrix Double -> Matrix Double -> [(Double, Vector Double)]
nonZeroSortedEigen epsilon d l = e
    where
          -- compute the 1/sqrt(degree) of each node.
          safeInvSqrt v = if 0 == v then 0 else 1 / sqrt v
          d12 = mapMatrix safeInvSqrt d
          -- compute eigen of the laplacian normalized by d12
          -- eigenvalues are sorted ascending
          -- eigenvectors is a matrix of column vectors
          (eigenvalues, eigenvectors) = eigS' (d12 <> (l <> d12))
          -- discard eigenvectors with a zero eigenvalue
          isTooNearzero (val, _) = val < epsilon
          e = dropWhile isTooNearzero $ zip (toList eigenvalues)
                                            (toColumns eigenvectors)


-- Compute the signal flow directly from an adjacency matrix
signalFlow :: Matrix Double -> Vector Double
signalFlow am = z
    where 
          -- transpose
          t = trans am
          -- symmetrized adjacency matrix
          w = (am + t) / 2
          -- diagonalized circuit graph degrees (number of edges per node)
          d = diagl $ map (foldVector (+) 0) $ toRows w
          -- graph Laplacian
          l = d - w
          -- matrix with the sign of the I/O differences of each node
          sign v = if v < 0 then -1 else 1
          signs = mapMatrix sign (am - t)
          -- column vector containing the row sums of the symmetrized adjacency matrix, qualified by the sign of the I/O differences
          b = (rows am) >< 1 $ map (foldVector (+) 0) $ toRows $ w * signs
          -- signal flow: matrix mutiplication of pseudoinverse of laplacian and b
          z = flatten $ (pinv l) <> b

