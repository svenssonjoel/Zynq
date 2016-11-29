
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
          let pullArr = pullMemory lmem 0 10 :: Pull (Expr Int) 

          -- Perform the computation on the pull array
          -- TODO: Implement basic set of pull array functions 
          
          i =: (i + 1) `mod` 10   -- Hide much of this in higher level abstractions
          sput out a 
 
