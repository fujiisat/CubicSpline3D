{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE ConstraintKinds #-}

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
import           Linear.Metric       (distance, Metric)
import           Linear.V3
import           Linear.Vector

-- | 3D Parametric cubic spline coefficients
data Spline v f = Spline { c3 :: V.Vector (v f)
                         , c2 :: V.Vector (v f)
                         , c1 :: V.Vector (v f)
                         , c0 :: V.Vector (v f)
                         , tj :: V.Vector f
                         } deriving (Show)

type Points v f = [(f, v f)]

-- | Constraint kinds for Linear.Vector of real number
type FloatVector v f = (R1 v, Functor v,  Floating f, V.Unbox f, V.Unbox (v f), Num (v f))

-- | Caliculate spline coefficients
spline :: FloatVector v f => Points v f-> Spline v f
spline points = Spline a b c d ts
  where
    (ts, ps) = (\(tl, pl) -> (V.fromList tl, V.fromList pl)) $ unzip points
    n = V.length ts - 1
    dt = V.zipWith (-) (V.tail ts) ts
    dp = V.zipWith (-) (V.tail ps) ps
    v = V.zipWith4 (\tp tm pp pm -> 6 *^ ((pp ^/ tp) - (pm ^/ tm))) (V.tail dt) dt (V.tail dp) dp
    mat = map (makeMatrixVec (n - 2) dt v) [0 .. n - 2]
    u = flip V.snoc 0 $ V.cons 0 $ tdma mat
    b = V.map (^/ 2) u
    d = ps
    a = V.zipWith3 (\ta up um -> (up - um) ^/ (6 * ta)) dt (V.tail u) u
    c = V.zipWith4 (\tc pc up um -> pc ^/ tc - tc *^ (2 *^ um + up) ^/ 6) dt dp (V.tail u) u

-- | Build a row of equation matrix
makeMatrixVec :: FloatVector v f => Int -> V.Vector f -> V.Vector (v f) -> Int -> (V.Vector f, v f)
makeMatrixVec n h d i
  | n == 0    = (V.fromList [2 * (h!i + h!(i+1)) ,       0,     0], d!i)
  | i == 0    = (V.fromList [2 * (h!i + h!(i+1)) , h!(i+1),     0], d!i)
  | i == n-1  = (V.fromList [2 * (h!i + h!(i+1)) ,       0,  -h!i], d!i)
  | otherwise = (V.fromList [2 * (h!i + h!(i+1)) , h!(i+1),  -h!i], d!i)

-- | Solve tridiagonal equation using TDMA algorithm
tdma :: FloatVector v f => [(V.Vector f, v f)] -> V.Vector (v f)
tdma [(m, m3)] = V.singleton (m3 ^/ m!0)
tdma mat = V.fromList $ foldl substitution [snd pq0] pq
  where
    (m0, m03) : ms = mat
    pq0 : pq = foldl elimination [(m0!1 / m0!0, m03 ^/ m0!0)] ms

-- | Forward elimination loop of TDMA
elimination :: FloatVector v f => [(f, v f)] -> (V.Vector f, v f) -> [(f, v f)]
elimination pqs@((pm, qm): _) (m, m3) = (p, q) : pqs
  where
    !p =  m!1 / (m!0 - m!2 * pm)
    !q = (m3 + m!2 *^ qm) ^/ (m!0 - m!2 * pm)
elimination [] _ = error "error elimination"

-- | Backward substitution loop of TDMA
substitution :: FloatVector v f => [v f] -> (f, v f) -> [v f]
substitution ts@(tp: _) (pj, qj) = t : ts
  where
    !t = pj *^ tp + qj
substitution [] _ = error "error substitution"

-- | Caliculate an interpolated value within the range
sample :: (FloatVector v f, Show f, Ord f) => Spline v f -> f -> Either String (v f)
sample (Spline a b c d ts) t
  | t `isInRangeOf` ts = Right $ (((a!i ^* t') + b!i) ^* t' + c!i) ^* t' + d!i
  | otherwise          = Left $ "Out of bounds t = " <> show t
  where
    i = fromMaybe (V.length ts - 1) (V.findIndex (t <) ts) - 1
    t' = t - ts!i
    delta = 7.450580596923828e-9
    y `isInRangeOf` ys = y >= V.head ys - delta && y <= V.last ys + delta

-- | Parameter by path distance
paramByDistance :: (FloatVector v f, Metric v) => [v f] -> [f]
paramByDistance points = scanl' (+) 0 (zipWith distance (tail points) points)
