module Utils
    (
      getData
    , getStrData
    , getHandle
    , numFilter
    , streamFormat
    ) where

import System.IO
import Data.List (intercalate)
import Data.String.Utils (replace)

-- return a list of floats from the input file string
getData :: String -> [Float]
getData instr = map (\x -> read x :: Float) (words fstr)
              where fstr = filter numFilter instr

-- string representation of data in list form
getStrData :: String -> String
getStrData instr = "[" ++ intercalate "," (words fstr) ++ "]"
                   where fstr = filter numFilter instr

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
-- delimiters and outputs 1 number per line
streamFormat :: String -> String
streamFormat s = replace "," "\n" ((init . tail) s)