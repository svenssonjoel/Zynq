

module MemoryRead where

import Zincite.Syntax
import Zincite.GenCpp

addMem :: InterfaceIO -> Compute ()
addMem io =
  do
    mwrite io address1 $ (mread io address2 + mread io (address2 + 4) :: Expr Int)

    where address1 = 10000 :: Expr Int
          address2 = 20000 :: Expr Int
    



addMemAST =
  let io = (InterfaceIO "io")
  in snd $ runCompute $ addMem io  

