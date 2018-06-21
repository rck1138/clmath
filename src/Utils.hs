{-# LANGUAGE DeriveDataTypeable #-}
module Utils
    (
      getData
    , getStrData
    , getHandle
    , numFilter
    ) where

import System.IO
import Data.List (intercalate)

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

