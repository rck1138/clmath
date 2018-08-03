module Utils
    (
      getData
    , getDataLOS
    , getStrData
    , getHandle
    , numFilter
    , streamFormat
    , strPred
    ) where

import System.IO
import Data.List (intercalate)
import Data.String.Utils (replace)

-- return a list of floats from the input file string
getData :: String -> [Float]
getData instr = map (\x -> read x :: Float) (words fstr)
              where fstr = filter numFilter instr

-- string representation of data as a list of strings
getDataLOS :: String -> String
getDataLOS instr = "[\"" ++ intercalate "\",\"" (lines instr) ++ "\"]"

-- string representation of data as a list of values
getStrData :: String -> String
getStrData instr = "[" ++ intercalate "," (words fstr) ++ "]"
                   where fstr = filter numFilter instr

-- create a predicate to operate on a string column
strPred :: Int -> (Float -> Bool) -> String -> Bool
strPred c p s = p (read ( f c s ):: Float)
   where f n = (!!(n-1)) . words

-- get a handle to the file we want to process
getHandle :: FilePath -> IO Handle
getHandle fp | fp == "std" = return stdin
             | otherwise = openFile fp ReadMode

-- provide basic filtering to remove non-numeric input
numFilter :: Char -> Bool
numFilter = (\x -> x `elem` chars)
            where chars = ['0','1','2','3','4','5',
                           '6','7','8','9','.','e',
                           'E','-','+',' ','\n']

-- formating function used from stream mode. Removes the list
-- delimiters, quotes and commans, and outputs 1 item per line
streamFormat :: String -> String
streamFormat s = (remove "]") . (remove "[") . (remove "\"") . (replace "," "\n") $ s
               where remove c = replace c ""