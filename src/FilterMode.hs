module FilterMode
    ( 
      filtStream
    ) where

import System.IO (hGetContents)
import CLMath
import Interpret (interpret)
import Utils (getStrData, getHandle, numFilter, streamFormat)

-- filter an input stream by applying a user-supplied 
-- predicate to each stream element before output
filtStream :: CLMath -> IO()
filtStream args = do
  let p = (pred_ args)
  c <- getHandle (file_ args) >>= hGetContents
  let expr = "filter " ++ p ++ " " ++ getStrData c
  interpret expr streamFormat