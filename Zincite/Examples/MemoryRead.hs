
module MemoryRead where

import Zincite.Syntax
import Zincite.Backend.HLS

addMem :: Memory Global -> Compute ()
addMem io =
  do
    mwrite io address1 $ (mread io address2 + mread io (address2 + 4) :: ZInt)

    where address1 = 10000 :: ZAddress
          address2 = 20000 :: ZAddress
    



addMemAST =
  let io = M (InterfaceIO "io")
  in snd $ runCompute $ addMem io  

