module SortMode 
   (
    runSort
    ) where

import Data.List (intercalate, sortBy)
import System.IO (hGetContents)
import Utils (getHandle, numFilter)
import CLMath

runSort :: CLMath -> IO()
runSort args = do 
    let n = (col_ args) - 1 -- subtract one for 1-based indexing
    let f = fullMode (full_ args) n
    c <- getHandle (file_ args) >>= hGetContents
    putStrLn $ (intercalate "\n" . map f . (sortBy (orderByCol n)) . (filterLines n)) c

fullMode False n = (\x -> (words x)!!n)
fullMode True _ = id 

filterLines :: Int -> String -> [String]
filterLines n s = filter f (lines s)
   where f = (>n) . length . words

orderByCol :: Int -> String -> String -> Ordering
orderByCol n s1 s2
   | n1 > n2 = GT
   | n1 < n2 = LT
   | n1 == n2 = EQ
      where n1 = sToFConvert $ (words s1)!!n
            n2 = sToFConvert $ (words s2)!!n
            sToFConvert s = (read s) :: Float