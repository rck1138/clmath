-- CLMath.hs -- Type definitions
{-# LANGUAGE DeriveDataTypeable #-}
 
 module CLMath
   (
     CLMath (..)
   ) where

import System.Console.CmdArgs

data CLMath = Reduce { sum_  :: Bool
                     , min_  :: Bool
                     , max_  :: Bool
                     , avg_  :: Bool
                     , dev_  :: Bool
                     , file_ :: FilePath
                     } 
            | Expr { expression_ :: String }
              deriving (Data, Typeable, Show, Eq)