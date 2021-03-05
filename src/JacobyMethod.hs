module JacobyMethod
  ( jacobyMethod
  , debugJacobyMethod
  )
where

import qualified Data.Matrix                   as M

import qualified Data.Vector                   as V
import           Linear.Epsilon
import           Data.Either

jacobyMethod
  :: (Num d, Ord d, Fractional d, Show d, Epsilon d)
  => M.Matrix d
  -> V.Vector d
  -> V.Vector d
  -> V.Vector d
jacobyMethod a b x_initial =
  let l_plus_u = M.matrix (M.nrows a)
                          (M.ncols a)
                          (\(i, j) -> if i /= j then a M.! (i, j) else 0)
      rev_d = fromRight (M.zero (M.nrows a) (M.ncols a))
                        (M.inverse . M.diagonal 0 . M.getDiag $ a)
      t     = negate <$> rev_d * l_plus_u
      c     = rev_d * M.colVector b
      x_new = M.getMatrixAsVector $ t * M.colVector x_initial + c
  in  if all (nearZero . abs) (V.zipWith (-) x_initial x_new)
        then x_new
        else jacobyMethod a b x_new

debugJacobyMethod
  :: (Num d, Ord d, Fractional d, Show d, Epsilon d)
  => M.Matrix d
  -> V.Vector d
  -> V.Vector d
  -> IO ()
debugJacobyMethod a b x_initial = do
  let l = M.matrix (M.nrows a)
                   (M.ncols a)
                   (\(i, j) -> if i > j then a M.! (i, j) else 0)
  let u = M.matrix (M.nrows a)
                   (M.ncols a)
                   (\(i, j) -> if i < j then a M.! (i, j) else 0)
  let l_plus_u = M.matrix (M.nrows a)
                          (M.ncols a)
                          (\(i, j) -> if i /= j then a M.! (i, j) else 0)
  let rev_d = fromRight (M.zero (M.nrows a) (M.ncols a))
                        (M.inverse . M.diagonal 0 . M.getDiag $ a)
  let t     = negate <$> rev_d * l_plus_u
  let c     = rev_d * M.colVector b
  let x_new = M.getMatrixAsVector $ t * M.colVector x_initial + c

  let sums1 = V.generate
        (length b)
        (\i ->
          ( (M.rowVector . V.slice 0 (i + 1) . M.getRow (i + 1) $ a)
            * (M.colVector . V.slice 0 (i + 1) $ x_initial)
            )
            M.! (1, 1)
        )

  let sums2 = V.generate
        (length b)
        (\i ->
          ( (M.rowVector . V.slice i (length b - i) . M.getRow (i + 1) $ a)
            * (M.colVector . V.slice i (length b - i) $ x_initial)
            )
            M.! (1, 1)
        )
  if all (nearZero . abs) (V.zipWith (-) x_initial x_new)
    then print x_new
    else debugJacobyMethod a b x_new
