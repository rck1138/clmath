{-# LANGUAGE DeriveDataTypeable #-}
 
 module ReduceMode
    (
      runReduce
    ) where

import System.IO
import Numeric.Statistics (average, avgdev)
import CLMath
import Utils (numFilter)

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

-- get a handle to the file we want to process
getHandle :: FilePath -> IO Handle
getHandle fp | fp == "std" = return stdin
             | otherwise = openFile fp ReadMode

-- return a list of floats from the input file string
getData :: String -> [Float]
getData instr = map (\x -> read x :: Float) (words fstr)
              where fstr = filter numFilter instr

applyFuncs :: [([Float] -> String)] -> [Float] -> [String]
applyFuncs (op:ops) num_list = (op num_list) : applyFuncs ops num_list
applyFuncs _ _           = []

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
