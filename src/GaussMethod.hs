module GaussMethod
  ( gaussMethod
  )
where

import qualified Data.Matrix                   as M

import qualified Data.Vector                   as V


gaussMethod
  :: (Num d, Ord d, Fractional d) => M.Matrix d -> V.Vector d -> V.Vector d
gaussMethod a b =
  let
    Just (u, l, p, d) = M.luDecomp a
    pxb               = V.fromList . M.toList $ p * M.colVector b
    helperY 0 = V.singleton (pxb V.! 0)
    helperY n =
      helperY (n - 1)
        `V.snoc` (pxb V.! n - sum
                   ( (M.rowVector . V.slice 0 n $ M.getRow (n + 1) l)
                   * M.colVector (helperY (n - 1))
                   )
                 )
    y     = helperY (length pxb - 1)
    uDiag = M.getDiag u
    helperX 0 = V.singleton (V.last y / V.last uDiag)
    helperX n =
      (   (y V.! (V.length y - (n + 1)) - sum
            ((M.rowVector . V.slice (length y - n) n $ M.getRow (length y - n) u)
            * M.colVector (helperX (n - 1))
            )
          )
        /   uDiag
        V.! (V.length y - (n + 1))
        )
        `V.cons` helperX (n - 1)
    x = helperX (length pxb - 1)
  in
    x


debugGaussMethod
  :: (Num d, Ord d, Fractional d, Show d) => M.Matrix d -> V.Vector d -> IO ()
debugGaussMethod a b = do
  let Just (u, l, p, _) = M.luDecomp a
  let pxb               = V.fromList $ M.toList (p * M.colVector b)
  let helperY 0 = V.singleton (pxb V.! 0)
  let helperY n =
        helperY (n - 1)
          `V.snoc` (pxb V.! n - sum
                     ( (M.rowVector . V.slice 0 n $ M.getRow (n + 1) l)
                     * M.colVector (helperY (n - 1))
                     )
                   )
  let y     = helperY (length pxb - 1)
  let uDiag = M.getDiag u
  let helperX 0 = V.singleton (V.last y / V.last uDiag)
  let
    helperX n =
      (   (y V.! (V.length y - (n + 1)) - sum
            ((M.rowVector . V.slice (length y - n) n $ M.getRow (length y - n) u)
            * M.colVector (helperX (n - 1))
            )
          )
        /   uDiag
        V.! (V.length y - (n + 1))
        )
        `V.cons` helperX (n - 1)
  let x = helperX (length pxb - 1)

  putStrLn . M.prettyMatrix . M.colVector $ x
