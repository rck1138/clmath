{-# LANGUAGE DeriveDataTypeable #-}
module Utils
    (
      numFilter
    ) where

-- provide basic filtering to remove non-numeric input
numFilter :: Char -> Bool
numFilter = (\x -> x `elem` chars)
            where chars = ['0','1','2','3','4','5',
                           '6','7','8','9','.','e',
                           'E','-','+',' ','\n']