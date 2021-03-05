module Lib
  ( printGaussMethod
  , printJacobyMethod
  , printSeidelMethod
  )
where

import           GHC.Float
import           System.Random
import qualified Data.Matrix                   as M

import qualified Data.Vector                   as V

import           GaussMethod
import           JacobyMethod
import           SeidelMethod
import           Data.List

-- randomVector n =
--   let roll    = uniformR (1, 6) :: RandomGen g => g -> (Int, g)
--       rolls   = unfoldr (Just . roll) :: RandomGen g => g -> [Int]
--       pureGen = mkStdGen 42
--       results = take n (rolls pureGen) :: [Int]
--   in  fromList $ results

testList = (int2Double <$>) <$> [[1, 2, 0], [3, 4, 4], [5, 6, 3]]

testJacobyList = (int2Double <$>) <$> [[2, 1, 0], [3, 8, 4], [5, 3, 9]]

testWikiJacobyList = (int2Double <$>) <$> [[2, 1], [5, 7]]
testWikiJacobyVector = V.fromList $ int2Double <$> [11, 13]
testWikiJacobyXInit = V.fromList $ int2Double <$> [1, 1]
testVector = V.fromList $ int2Double <$> [3, 7, 8]



randomVector n =
  V.fromList ((take n . unfoldr (Just . randomR (0, 31))) (mkStdGen 137))

randomSquareMatrix n = M.fromList
  n
  n
  ((take (n * n) . unfoldr (Just . randomR (0, 31))) (mkStdGen 137))

randomJacobyMatrix n = M.matrix
  n
  n
  (\(i, j) -> if i /= j then randMatrix M.! (i, j) else newDiag V.! (i - 1))
 where
  randMatrix = randomSquareMatrix n
  newDiag    = V.zipWith (\a b -> a - b + 1)
                         (V.fromList (sum <$> M.toLists randMatrix))
                         (M.getDiag randMatrix)

generateMatrix = M.matrix 4 4 $ int2Double . uncurry (+)
hilbertMatrix n =
  M.matrix n n (\(i, j) -> 1.0 / (int2Double i + int2Double j - 1.0))


printColVector :: (Show a) => V.Vector a -> IO ()
printColVector = putStrLn . M.prettyMatrix . M.colVector

printGaussMethod :: IO ()
printGaussMethod = printColVector
  (gaussMethod (randomSquareMatrix 20) (randomVector 20) :: V.Vector Double)

printJacobyMethod :: IO ()
printJacobyMethod = printColVector
  (jacobyMethod (randomJacobyMatrix 5)
                (randomVector 5)
                (V.replicate (V.length (randomVector 5 :: V.Vector Double)) 0) :: V.Vector
      Double
  )

printSeidelMethod :: IO ()
printSeidelMethod = printColVector
  (seidelMethod
    (randomJacobyMatrix 20)
    (randomVector 20)
    (V.replicate (V.length (randomVector 20 :: V.Vector Double)) 0) :: V.Vector
      Double
  )



-- someFunc = putStrLn . prettyMatrix $ gaussMethod
--   (hilbertMatrix 5)
--   (int2Double <$> randomVector 5)

-- someFunc = debugGaussMethod (Data.Matrix.fromLists testList)
--                             (Data.Vector.fromList testVector)

--   print
--     . fmap (prettyMatrix . (\(u, l, p, d) -> l))
--     . gaussMethod
--     $ generateMatrix
