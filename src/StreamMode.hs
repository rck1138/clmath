{-# LANGUAGE DeriveDataTypeable #-}

module StreamMode
    (
      procStream
    ) where

import qualified Language.Haskell.Interpreter as I
import Utils (numFilter)

procStream :: [String] -> IO()
