
{-# LANGUAGE ScopedTypeVariables #-} 

module StreamMedian where

import Zincite.Syntax
import Zincite.GenCpp
import Zincite.Pull

import Prelude hiding (mod)


-- TODO Implement tuple based window using bram
-- TODO Implement a circular buffer abstraction on top of bram
-- TODO Needs a way to handle counters (nicely) 
streamsMedian :: StreamIn Int -> StreamOut Int -> Compute ()
streamsMedian in1 out =
  do lmem <- bram (10*4) -- 10 * 4byte quantities
     (i :: Expr Address) <- declare
     i =: 0
     forever $ 
       do 
          a <- sget in1
          
          -- Write to memory
          mwrite lmem i a
          -- create a 10 element pull array from the bram e
          --let pullArr = pullMemory lmem 0 10 :: Pull (Expr Int) 

          -- a better abstraction than pull arrays when dealing with statically known
          -- amounts of data (that we want to do "unrolled" computations on, is a normal Haskell List
          let list = [ mread lmem (fromInteger ix) :: Expr Int|  ix <- [0..9]] 
     
              -- Perform the computation on the list 

              -- Cheating by performing a sum operation instead of a Median operation
              result = sum list -- Haskell's built in sum (metaprogramming!) 
          
          i =: (i + 1) `mod` 10   -- Hide much of this in higher level abstractions
          sput out result  
 
