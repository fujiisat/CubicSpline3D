{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.List                  (intercalate)
import           Data.Monoid                ((<>))
import qualified Data.Vector.Unboxed        as V
import           Linear.Metric              (distance)
import           Linear.V2
import           System.Environment         (getArgs, getProgName)

import           Math.CubicSpline3D
main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> do
      let n = read arg
      points <- readInputs <$> B8.getContents
      let dists = paramByDistance points
      let step = last dists / fromIntegral n
      let sp = spline $ zip  dists  points
      B8.putStrLn $ B8.unlines $ showSample $ mapSample sp $ take (n+1) $ map (step * ) [0..]
    _ -> do
      prog <- getProgName
      putStrLn $ prog <> " num < input.csv > output.csv"

-- | Read list of coordinate data
readInputs :: B8.ByteString -> [V2 Double]
readInputs = map readDoubleXYZ . B8.lines

-- | Read comma separated coordinates (x, y, z)
-- Exception in case of no parse or number of data is less than one
-- If number of data is two, z is assumed to be 0
readDoubleXYZ :: B8.ByteString -> V2 Double
readDoubleXYZ bs = V2 (read $ B8.unpack z) (read $ B8.unpack y)
  where
    x:y:rest = B8.split ',' bs
    z = if null rest then "0" else head rest

-- |
mapSample :: Spline V2 Double -> [Double] -> [(Double, V2 Double)]
mapSample sp = foldr  f []
  where
    f x acc = case sample sp x of
                Right v -> (x, v) : acc
                Left s  -> error $ "error" ++ s  --acc

-- | Show sample results
showSample :: [(Double, V2 Double)] -> [B8.ByteString]
showSample = map (\(t, V2  y z) -> B8.pack $ intercalate "," $ map show [t,  y ,z])
