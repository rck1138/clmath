module StreamMode
    (
      procStream
    ) where

import System.IO (hGetContents)
import CLMath
import Interpret (interpret)
import Utils (getStrData, getHandle, numFilter, streamFormat)

-- process an input stream by applying a user-supplied 
-- function to each stream element before output
procStream :: CLMath -> IO()
procStream args = do
  let f = (func_ args)
  c <- getHandle (file_ args) >>= hGetContents
  let expr = "map " ++ f ++ " " ++ getStrData c
  interpret expr streamFormat