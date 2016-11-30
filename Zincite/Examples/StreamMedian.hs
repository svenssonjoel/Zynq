
{-# LANGUAGE ScopedTypeVariables #-} 

module StreamMedian where

import Zincite.Syntax
import Zincite.GenCpp
import Zincite.Lava

import Prelude hiding (mod,div)



-- TODO Implement a circular buffer abstraction on top of bram
-- TODO Needs a way to handle counters (nicely) 

streamsMedian :: StreamIn Int -> StreamOut Int -> Compute ()
streamsMedian in1 out =
  do lmem <- bram (8*4) -- 10 * 4byte quantities
     (i :: Expr Address) <- declare
     i =: 0
     forever $ 
       do 
          a <- sget in1
          
          -- Write to memory
          mwrite lmem i a

          -- a better abstraction than pull arrays when dealing with statically known
          -- amounts of data (that we want to do "unrolled" computations on, is a normal Haskell List
          let sorted = sortB 3 twosort $ listFromMem lmem 0 8
              result = (sorted !! 3 + sorted !! 4) `div` 2 -- or go float ? 
          
          i =: (i + 1) `mod` 10   -- Hide much of this in higher level abstractions
          sput out result  
 
