{-# LANGUAGE DeriveDataTypeable #-}

module ExprMode
    (
      evalExpr
    ) where

import qualified Language.Haskell.Interpreter as I
import Data.List (intercalate)
import Control.Monad.IO.Class (liftIO)

evalExpr :: String -> IO ()
evalExpr str = do r <- I.runInterpreter (exprInt str)
                  case r of
                    Left err -> putStrLn $ errorString err
                    Right () -> return ()       

exprInt :: String -> I.Interpreter ()
exprInt str =
    do
      I.setImports ["Prelude"]
      say $ "Evaluating: " ++ str
      a <- I.eval str
      say $ show a

say :: String -> I.Interpreter ()
say = liftIO . putStrLn

errorString :: I.InterpreterError -> String
errorString (I.WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (I.GhcError e) = e
errorString e = show e