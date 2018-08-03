module Interpret
    (
      interpret
    , interpFilterExpr
    , ife
    , errorString
    ) where

import qualified Language.Haskell.Interpreter as I
import Data.List (intercalate)
--import Control.Monad (liftIO)

-- Interpret an input string, apply a formatting function, and output result
interpret :: String -> (String -> String) -> IO ()
interpret str fmt = do r <- I.runInterpreter (exprInt str)
                       case r of
                         Left err -> putStrLn $ (errorString err) ++ "\n" ++ str
                         Right s -> putStrLn $ fmt s      

-- take and expression and return an interpreter ready to evaluate it
exprInt :: String -> I.Interpreter (String)
exprInt str =
    do
      I.setImports ["Prelude"]
      a <- I.eval str
      return a

interpFilterExpr :: String -> String -> I.Interpreter ([Float])
interpFilterExpr func dat =
    do 
      I.setImports ["Prelude"]
      p <- I.interpret func (I.as :: Float -> Bool)
      d <- I.interpret dat (I.as :: [Float])
      --I.liftIO . putStrLn $ show (p 1024)
      --I.liftIO . putStrLn $ show dat
      return $ filter p d

ife :: String -> String -> IO ()
ife func dat = do r <- I.runInterpreter (interpFilterExpr func dat)
                  case r of
                    Left err -> putStrLn $ (errorString err) ++ "\n" ++ dat
                    Right s -> putStrLn $ show s

-- produce an error if the exrepssion can't be compiled
errorString :: I.InterpreterError -> String
errorString (I.WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Input Won't compile:"
    unbox (I.GhcError e) = e
errorString e = show e

--applyFunc :: String -> IO ()
--applyFunc s = do r <- I.runInterpreter