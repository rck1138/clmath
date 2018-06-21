{-# LANGUAGE DeriveDataTypeable #-}

module StreamMode
    (
      procStream
    ) where

import Data.List (intercalate)
import Data.String.Utils (replace)
import System.IO
import qualified Language.Haskell.Interpreter as I
import Control.Monad.IO.Class (liftIO)
import CLMath
import Utils (getStrData, getHandle, numFilter)

procStream :: CLMath -> IO()
procStream args = do
  let f = (func_ args)
  c <- getHandle (file_ args) >>= hGetContents
  let expr = "map " ++ f ++ " " ++ getStrData c
  evalExpr expr
  --putStrLn $ intercalate "\n" (map show (getData c))

evalExpr :: String -> IO ()
evalExpr str = do r <- I.runInterpreter (exprInt str)
                  case r of
                    Left err -> putStrLn $ errorString err
                    Right s -> putStrLn $ replace "," "\n" ((init . tail) s)      

exprInt :: String -> I.Interpreter (String)
exprInt str =
    do
      I.setImports ["Prelude"]
      a <- I.eval str
      return a

say :: String -> I.Interpreter ()
say = liftIO . putStrLn

errorString :: I.InterpreterError -> String
errorString (I.WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (I.GhcError e) = e
errorString e = show e