-- clmath -- simple command line math
{-# LANGUAGE DeriveDataTypeable #-}
module Lib
    ( libMain
    ) where

--import System.Environment (getArgs)
import System.Console.CmdArgs
import System.IO
import Numeric.Statistics (average, avgdev)

data CLMath = CLMath { sum_  :: Bool
                     , min_  :: Bool
                     , max_  :: Bool
                     , avg_  :: Bool
                     , dev_  :: Bool
                     , file_ :: FilePath
                     } deriving (Data, Typeable, Show, Eq)

clmath = CLMath 
         { sum_  = False &= help "Report sum of numbers in input list" 
         , min_  = False &= name "m" &= help "Report minimum number in input list"
         , max_  = False &= name "x" &= help "Report maximum number in input list"
         , avg_  = False &= help "Report mean value of numbers in input list"
         , dev_  = False &= help "Report deviation of the mean for number in input list"
         , file_ = "std" &= args &= typ "FILE" 
         } &=
         verbosity &=
         help "Simple math on the command line" &=
         summary "clmath v0.0.1, (C) Rory Kelly" &=
         details ["clmath performs simple operations on a list of numbers",""
                 ,"For example, to sum a list from stdin:","  cat numbers.txt | clmath --sum"]

mode = cmdArgsMode clmath

-- -- -- This function gets passed to main -- -- -- 
libMain :: IO ()
libMain = do 
    args <- cmdArgs clmath
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

-- provide basic filtering of the input
numFilter :: Char -> Bool
numFilter = (\x -> x `elem` chars)
            where chars = ['0','1','2','3','4','5',
                           '6','7','8','9','.','e',
                           'E','-','+',' ','\n']

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
