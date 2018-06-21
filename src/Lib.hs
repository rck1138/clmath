-- clmath -- simple command line math
{-# LANGUAGE DeriveDataTypeable #-}
module Lib
    (
      libMain
    ) where

import System.Console.CmdArgs
import CLMath
import ExprMode
import ReduceMode
import StreamMode

-- 
expr = Expr
       { expression_ = def &= typ "EXPRESSION" &= args
       } &= help "Evaluate a simple arithmetic expression"

reduce = Reduce 
         { sum_  = False &= help "Report sum of numbers in input list" 
         , min_  = False &= name "m" &= help "Report minimum number in input list"
         , max_  = False &= name "x" &= help "Report maximum number in input list"
         , avg_  = False &= help "Report mean value of numbers in input list"
         , dev_  = False &= help "Report deviation of the mean for numbers in input list"
         , file_ = "std" &= args &= typ "FILE"
         } &= auto 
           &= help "Perform reductions on a stream of numbers"

stream = Stream
         { func_ = def &= typ "FUNCTION" &= argPos 0
         , file_ = "std" &= args &= typ "FILE" &= argPos 1 &= opt "std"
         } &= help "Apply a function to each member of an input stream"


mode = cmdArgsMode $ modes [expr, reduce, stream] &= summary "clmath v0.0.1, (C) Rory Kelly"
       &= verbosity
       &= help "Simple math on the command line"
       &= summary "clmath v0.0.1, (C) Rory Kelly"

-- -- -- This function gets passed to main -- -- -- 
libMain :: IO ()
libMain = cmdArgsRun mode >>= runCLMath

-- process --
runCLMath :: CLMath -> IO()
runCLMath args@(Expr e) = evalExpr args
runCLMath args@(Stream fn fi) = procStream args
runCLMath args@(Reduce sm mn mx av dv fi) = runReduce args



