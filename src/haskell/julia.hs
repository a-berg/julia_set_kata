-- Julia.hs -- source code for computing Julia sets in Haskell
import Data.Complex

escapeTime :: (Complex -> Complex) -> Complex -> Int
escapeTime f z = go f z 0
  where
    go f z n
      | n == 100 = n -- maximum allowed iteration
      | abs nextz > 2.0 = n -- radius check
      | otherwise = go f nextz (n+1) -- normal iteration
      where nextz = f z

poly :: Complex -> Complex -> Complex
poly c z= z^2 + c


escapeTime (poly 0.6 +: 0.4) (0.0+:0.0)