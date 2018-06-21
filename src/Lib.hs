-- clmath -- simple command line math
module Lib
    (
      libMain
    ) where

import System.Console.CmdArgs
import CLMath
import ExprMode
import FilterMode
import ReduceMode
import StreamMode

-- definition of modes --
reduce = Reduce 
         { sum_  = False &= help "Report sum of numbers in input list" 
         , min_  = False &= name "m" &= help "Report minimum number in input list"
         , max_  = False &= name "x" &= help "Report maximum number in input list"
         , avg_  = False &= help "Report mean value of numbers in input list"
         , dev_  = False &= help "Report deviation of the mean for numbers in input list"
         , file_ = "std" &= args &= typ "FILE"
         } &= auto  -- default mode
           &= help "Perform reductions on a stream of numbers"

expr = Expr
       { expression_ = def &= typ "EXPRESSION" &= args
       } &= help "Evaluate a simple arithmetic expression"

filt = Filter
       { pred_ = def &= typ "PREDICATE" &= argPos 0
       , file_ = "std" &= typ "FILE" &= argPos 1 &= opt "std"
       } &= help "Filter an input stream with the supplied predicate"

stream = Stream
         { func_ = def &= typ "FUNCTION" &= argPos 0
         , file_ = "std" &= typ "FILE" &= argPos 1 &= opt "std"
         } &= help "Apply a function to each member of an input stream"


mode = cmdArgsMode $ modes [reduce, expr, filt, stream]
       &= verbosity
       &= help "Simple math from the command line" 
       &= summary "clmath v0.1.0, (C) Rory Kelly"

-- -- -- This function gets passed to main -- -- -- 
libMain :: IO ()
libMain = cmdArgsRun mode >>= runCLMath

-- run program in requested mode --
runCLMath :: CLMath -> IO()
runCLMath args@(Expr _) = evalExpr args
runCLMath args@(Filter _ _) = filtStream args
runCLMath args@(Stream _ _) = procStream args
runCLMath args@(Reduce _ _ _ _ _ _) = runReduce args



