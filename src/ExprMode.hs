{-# LANGUAGE DeriveDataTypeable #-}

module ExprMode
    (
      evalExpr
    ) where

import CLMath
import Interpret (interpret)

evalExpr :: CLMath -> IO ()
evalExpr (Expr e) = interpret e id
