module SeidelMethod
  ( seidelMethod
  )
where

import qualified Data.Matrix                   as M

import qualified Data.Vector                   as V
import           Linear.Epsilon
import           Data.Either                    ( fromRight )


seidelMethod
  :: (Num d, Ord d, Fractional d, Epsilon d)
  => M.Matrix d
  -> V.Vector d
  -> V.Vector d
  -> V.Vector d
seidelMethod a b x_initial =
  let
    l = M.matrix (M.nrows a)
                 (M.ncols a)
                 (\(i, j) -> if i >= j then a M.! (i, j) else 0)
    u = M.matrix (M.nrows a)
                 (M.ncols a)
                 (\(i, j) -> if i < j then a M.! (i, j) else 0)
    rev_l = fromRight (M.zero (M.nrows a) (M.ncols a)) (M.inverse l)
    x_new =
      M.getMatrixAsVector $ rev_l * (M.colVector b - u * M.colVector x_initial)
  in
    if all (nearZero . abs) (V.zipWith (-) x_initial x_new)
      then x_new
      else seidelMethod a b x_new

debugSeidelMethod
  :: (Num d, Ord d, Fractional d, Epsilon d, Show d)
  => M.Matrix d
  -> V.Vector d
  -> V.Vector d
  -> IO ()
debugSeidelMethod a b x_initial = do
  let l = M.matrix (M.nrows a)
                   (M.ncols a)
                   (\(i, j) -> if i >= j then a M.! (i, j) else 0)
  let u = M.matrix (M.nrows a)
                   (M.ncols a)
                   (\(i, j) -> if i < j then a M.! (i, j) else 0)
  let rev_l = fromRight (M.zero (M.nrows a) (M.ncols a)) (M.inverse l)
  let x_new =
        M.getMatrixAsVector
          $ rev_l
          * (M.colVector b - u * M.colVector x_initial)

  putStrLn . M.prettyMatrix . M.colVector $ x_new
