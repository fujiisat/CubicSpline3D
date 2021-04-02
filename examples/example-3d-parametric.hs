{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.List                  (intercalate)
import           Linear.V3
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
      let step  = last dists / fromIntegral n
      let sp    = spline $ zip dists points
      B8.putStrLn $ B8.unlines $ showSample $ mapSample sp $ take (n + 1) $ map
        (step *)
        [0 ..]
    _ -> do
      prog <- getProgName
      putStrLn $ prog <> " num < input.csv > output.csv"

-- | Read list of coordinate data
readInputs :: B8.ByteString -> [V3 Double]
readInputs = map readDoubleXYZ . B8.lines

-- | Read comma separated coordinates (x, y, z)
-- Exception in case of no parse or number of data is less than one
-- If number of data is two, z is assumed to be 0
readDoubleXYZ :: B8.ByteString -> V3 Double
readDoubleXYZ bs = V3 (read $ B8.unpack x)
                      (read $ B8.unpack y)
                      (read $ B8.unpack z)
  where
    x : y : rest = B8.split ',' bs
    z            = if null rest then "0" else head rest

-- |
mapSample :: Spline V3 Double -> [Double] -> [(Double, V3 Double)]
mapSample sp = foldr f []
  where
    f x acc = case sample sp x of
      Right v -> (x, v) : acc
      Left  s -> error $ "error" ++ s  --acc

-- | Show sample results
showSample :: [(Double, V3 Double)] -> [B8.ByteString]
showSample =
  map (\(t, V3 x y z) -> B8.pack $ intercalate "," $ map show [t, x, y, z])
