module Lib
  ( printGaussMethod
  )
where


import           GaussMethod
import           GHC.Float
import           System.Random
import qualified Data.Matrix                   as M

import qualified Data.Vector                   as V


-- randomVector n =
--   let roll    = uniformR (1, 6) :: RandomGen g => g -> (Int, g)
--       rolls   = unfoldr (Just . roll) :: RandomGen g => g -> [Int]
--       pureGen = mkStdGen 42
--       results = take n (rolls pureGen) :: [Int]
--   in  fromList $ results

testList = (int2Double <$>) <$> [[1, 2, 0], [3, 4, 4], [5, 6, 3]]

testVector = int2Double <$> [3, 7, 8]

randomVector n = V.generate n id

generateMatrix = M.matrix 4 4 $ int2Double . uncurry (+)

hilbertMatrix n =
  M.matrix n n (\(i, j) -> 1.0 / (int2Double i + int2Double j - 1.0))

printGaussMethod :: IO ()

printGaussMethod = putStrLn . M.prettyMatrix . M.colVector $ gaussMethod
  (M.fromLists testList)
  (V.fromList testVector)



-- someFunc = putStrLn . prettyMatrix $ gaussMethod
--   (hilbertMatrix 5)
--   (int2Double <$> randomVector 5)

-- someFunc = debugGaussMethod (Data.Matrix.fromLists testList)
--                             (Data.Vector.fromList testVector)

--   print
--     . fmap (prettyMatrix . (\(u, l, p, d) -> l))
--     . gaussMethod
--     $ generateMatrix
