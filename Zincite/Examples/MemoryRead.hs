

module MemoryRead where

import Zincite.Syntax
import Zincite.GenCpp

addMem :: Memory Global -> Compute ()
addMem io =
  do
    mwrite io address1 $ (mread io address2 + mread io (address2 + 4) :: Expr Int)

    where address1 = 10000 :: Expr Address
          address2 = 20000 :: Expr Address
    



addMemAST =
  let io = M (InterfaceIO "io")
  in snd $ runCompute $ addMem io  

