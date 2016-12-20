{-# LANGUAGE GADTs, TypeOperators, DataKinds, KindSignatures #-}

import GHC.TypeLits


-- Empty dummy stream 
data Stream (l :: Symbol) a


data Connection (c :: [(Symbol, Symbol)]) 

data Box (istreams :: [*]) (ostreams :: [*]) where
  Iota :: Box '[] '[Stream l Int] 

  Dup :: Box '[Stream l a] '[Stream l1 a, Stream l2 a] 

  
  Connect :: Box i o -> Connection map -> Box i1 o1 -> Box i2 o2 


myIota :: Box '[] '[Stream "iota" Int]
myIota = Iota 

splitter :: Box '[Stream l a] '[Stream l1 a,Stream l2 a]
splitter = Dup 

--test1 = Box '[] '[Stream "out1" Int, Stream "out2" Int]
test1 = Connect myIota (undefined :: Connection '[("iota","in")]) splitter

