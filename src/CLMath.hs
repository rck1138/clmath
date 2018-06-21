-- CLMath.hs -- Type definitions
{-# LANGUAGE DeriveDataTypeable #-}
 
 module CLMath
   (
     CLMath (..)
   ) where

import System.Console.CmdArgs

data CLMath = Expr { expression_ :: String }
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
              deriving (Data, Typeable, Show, Eq)
