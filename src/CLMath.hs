-- CLMath.hs -- Type definitions
{-# LANGUAGE DeriveDataTypeable #-}
 
 module CLMath
   (
     CLMath (..)
   ) where

import System.Console.CmdArgs

-- CLMath types for each possible program mode
data CLMath = Expr { expression_ :: String }
            | Filter { pred_ :: String
                     , col_  :: Int
                     , full_ :: Bool
                     , file_ :: FilePath
                     }
            | Reduce { sum_  :: Bool
                     , min_  :: Bool
                     , max_  :: Bool
                     , avg_  :: Bool
                     , dev_  :: Bool
                     , file_ :: FilePath
                     } 
            | Stream { func_ :: String
                     , file_ :: FilePath
                     }
            | Sort   { col_  :: Int
                     , full_ :: Bool
                     , file_ :: FilePath
                     }
              deriving (Data, Typeable, Show, Eq)

