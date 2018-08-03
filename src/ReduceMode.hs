{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}
 
 module ReduceMode
    (
      runReduce
    ) where

import System.IO
import Data.List (foldl')
--import Numeric.Statistics (average, avgdev)
import CLMath
import Utils (getData, getHandle, numFilter)

runReduce :: CLMath -> IO ()
runReduce args = do
  c <- getHandle (file_ args) >>= hGetContents
  putStr $ concat (applyFuncs (getOps args) (getData c))

-- given the argument settings, produce a list of operations to apply
getOps :: CLMath -> [([Float] -> String)]
getOps a = (if sum_ a then [findSum] else [])
        ++ (if min_ a then [findMin] else [])
        ++ (if max_ a then [findMax] else [])
        ++ (if avg_ a then [findAvg] else [])
        ++ (if dev_ a then [findDev] else [])

applyFuncs :: [([Float] -> String)] -> [Float] -> [String]
applyFuncs (op:ops) num_list = (op num_list) : applyFuncs ops num_list
applyFuncs _ _           = []

-- |Numerically stable mean
mean :: Fractional a => [a] -> a
mean x = fst $ foldl' addElement (0,0) x
    where
      addElement (!m,!n) x = (m + (x-m)/(n+1), n+1)

-- |Same as 'mean'
average :: Fractional a => [a] -> a
average = mean

-- | Average deviation
avgdev :: (Floating a) => [a] -> a
avgdev xs = mean $ map (\x -> abs(x - m)) xs
    where
      m = mean xs

findSum :: [Float] -> String
findSum l = "Sum: " ++ show (sum l) ++ "\n"

findMin :: [Float] -> String
findMin l = "Min: " ++ show (minimum l) ++ "\n"

findMax :: [Float] -> String
findMax l = "Max: " ++ show (maximum l) ++ "\n"

findAvg :: [Float] -> String
findAvg l = "Average: " ++ show (average l) ++ "\n"

findDev :: [Float] -> String
findDev l = "Deviation: " ++ show (avgdev l) ++ "\n"
