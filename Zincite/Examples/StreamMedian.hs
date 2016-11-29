

module StreamMedian where

import Zincite.Syntax
import Zincite.GenCpp

streamsMedian :: StreamIn Int -> StreamOut Int -> Compute ()
streamsMedian in1 out =
  do lmem <- bram (10*4) -- 10 * 4byte quantities 
     forever $ 
       do a <- sget in1
          -- TODO Implement tuple based window using bram
          -- TODO Implement a circular buffer abstraction on top of bram
          -- TODO Needs a way to handle counters (nicely) 
          sput out a
 
