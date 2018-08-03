module FilterMode
    ( 
      filtStream
    ) where

import System.IO (hGetContents)
import Language.Haskell.Interpreter
import Utils (getDataLOS, getHandle, numFilter, strPred, streamFormat)
import Interpret (errorString)
import CLMath

-- filter an input stream by applying a user-supplied 
-- predicate to each stream element before output
filtStream :: CLMath -> IO()
filtStream args = do
  let p = (pred_ args)
  let c = (col_ args)
 -- let f = fullMode (full_ args) n
  d <- getHandle (file_ args) >>= hGetContents
  runFilterInterp c p (getDataLOS d)

-- make an interpreter for the filter mode. The interpreter will
-- take a user supplied predicate, and filter the input by applying 
-- the predicate to the requested column number
mkFilterInterp :: Int -> String -> String -> Interpreter ([String])
mkFilterInterp col func dat =
    do 
      setImports ["Prelude"]
      p <- interpret func (as :: Float -> Bool)
      d <- interpret dat (as :: [String])
      return $ filter (strPred col p) (fd d)
      where fd xs = filter ((>= col) . length . words) xs

-- run the interpreter for the filter mode
runFilterInterp :: Int -> String -> String -> IO ()
runFilterInterp col pred dat = 
    do 
      r <- runInterpreter (mkFilterInterp col pred dat)
      case r of
        Left err -> putStrLn $ (errorString err) ++ "\n" ++ dat
        Right s -> putStrLn $ streamFormat (show s)