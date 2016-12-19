
module MemoryRead where

import Zincite.Syntax
import Zincite.Backend.HLS

addMem :: Memory Global -> Control Address -> Control Address -> Compute ()
addMem io address1 address2 =
  do
    mwrite io address1 $ (mread io address2 + mread io (address2 + 4) :: ZInt)
   

addMemAST =
  let io = M (InterfaceIO "io")
  in snd $ runCompute $ addMem io address1 address2  
   where address1 = 10000 :: ZAddress
         address2 = 20000 :: ZAddress
 
