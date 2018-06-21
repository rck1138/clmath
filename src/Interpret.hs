module Interpret
    (
      interpret
    ) where

import qualified Language.Haskell.Interpreter as I
import Data.List (intercalate)

-- Interpret an input string, apply a formatting function, and output result
interpret :: String -> (String -> String) -> IO ()
interpret str fmt = do r <- I.runInterpreter (exprInt str)
                       case r of
                         Left err -> putStrLn $ errorString err
                         Right s -> putStrLn $ fmt s      

-- take and expression and return an interpreter ready to evaluate it
exprInt :: String -> I.Interpreter (String)
exprInt str =
    do
      I.setImports ["Prelude"]
      a <- I.eval str
      return a

-- produce an error if the exrepssion can't be compiled
errorString :: I.InterpreterError -> String
errorString (I.WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Input Won't compile:"
    unbox (I.GhcError e) = e
errorString e = show e