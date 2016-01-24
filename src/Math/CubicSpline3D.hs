{-# LANGUAGE BangPatterns #-}

module Math.CubicSpline3D (
  Spline(),
  spline,
  sample,
  paramByDistance
  ) where

import           Data.List           (scanl', sort)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import           Linear.Metric       (distance)
import           Linear.V3
import           Linear.Vector

-- | 3D Parametric cubic spline coefficients
data Spline = Spline { c3 :: V.Vector (V3 Double)
                     , c2 :: V.Vector (V3 Double)
                     , c1 :: V.Vector (V3 Double)
                     , c0 :: V.Vector (V3 Double)
                     , tj :: V.Vector Double
                     } deriving (Show)

type Points = [(Double,V3 Double)]

-- | Caliculate spline coefficients
spline :: Points -> Spline
spline points = Spline a b c d ts
  where
    (ts, ps) = (\(tl, pl) -> (V.fromList tl, V.fromList pl)) $ unzip points
    n = V.length ts - 1
    dt = V.zipWith (-) (V.tail ts) ts
    dp = V.zipWith (-) (V.tail ps) ps
    v = V.zipWith4 (\tp tm pp pm -> 6 * ((pp^/tp) - (pm^/tm))) (V.tail dt) dt (V.tail dp) dp
    mat = map (makeMatrixVec (n-2) dt v) [0 .. n-2]
    u = flip V.snoc 0 $ V.cons 0 $ tdma mat
    b = V.map (/2) u
    d = ps
    a = V.zipWith3 (\ta up um -> (up - um) ^/ (6 * ta)) dt (V.tail u) u
    c = V.zipWith4 (\tc pc up um -> pc/(v3 tc) - tc *^ (2 *^ um + up) / 6) dt dp (V.tail u) u

-- | Build a row of equation matrix
makeMatrixVec :: Int -> V.Vector Double -> V.Vector (V3 Double) -> Int -> V.Vector (V3 Double)
makeMatrixVec n h d i
  | n == 0     = V.fromList [v3 $ 2 * (h!i + h!(i+1)) ,              0,         0, d!i]
  | i == 0     = V.fromList [v3 $ 2 * (h!i + h!(i+1)) , v3 $ - h!(i+1),         0, d!i]
  | i == n-1   = V.fromList [v3 $ 2 * (h!i + h!(i+1)) ,              0, v3 $ -h!i, d!i]
  | otherwise  = V.fromList [v3 $ 2 * (h!i + h!(i+1)) , v3 $   h!(i+1), v3 $ -h!i, d!i]

-- |
v3 :: Double -> V3 Double
v3 d = V3 1 1 1 ^* d

-- | Solve tridiagonal equation using TDMA algorithm
tdma :: [V.Vector (V3 Double)] -> V.Vector (V3 Double)
tdma [m] = V.singleton (m!3 / m!0)
tdma mat = V.fromList $ foldl substitution [snd pq0] pq
  where
    m0 : ms = mat
    pq0 : pq = foldl elimination [(m0!1 / m0!0, m0!3 / m0!0)] ms

-- | Forward elimination loop of TDMA
elimination :: [(V3 Double, V3 Double)] -> V.Vector (V3 Double) -> [(V3 Double, V3 Double)]
elimination pqs@((pm, qm):_) m = (p, q) : pqs
  where
    !p =  m!1 / (m!0 - m!2 * pm)
    !q = (m!3 + m!2 * qm) / (m!0 - m!2 * pm)
elimination [] _ = error "error elimination"

-- | Backward substitution loop of TDMA
substitution :: [V3 Double] -> (V3 Double, V3 Double) -> [V3 Double]
substitution ts@(tp:_) (pj, qj) = t : ts
  where
    !t = pj * tp + qj
substitution [] _ = error "error substitution"

-- | Caliculate an interpolated value within the range
sample :: Spline -> Double -> Either String (V3 Double)
sample (Spline a b c d ts) t
  | t `isInRangeOf` ts = Right $ (((a!i ^* t') + b!i) ^* t' + c!i) ^* t' + d!i
  | otherwise          = Left $ "Out of bounds t = " <> show t
  where
    i = fromMaybe (V.length ts - 1) (V.findIndex (t<) ts) - 1
    t' = t - ts!i
    delta = 7.450580596923828e-9
    y `isInRangeOf` ys = y >= V.head ys - delta && y <= V.last ys + delta

-- | Parameter by path distance
paramByDistance :: [V3 Double] -> [Double]
paramByDistance points = scanl' (+) 0 (zipWith distance (tail points) points)

