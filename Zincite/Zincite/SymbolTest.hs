{-# LANGUAGE GADTs, TypeOperators, DataKinds, KindSignatures, PolyKinds #-}


import GHC.TypeLits


-- Empty dummy stream 
data Stream (l :: Symbol) a

data Connection (c :: [(Symbol, Symbol)])

{- 
  TODO:
   - unzip "Connections"
   - Figure out how to check if the Connection mapping is "OK"
     for 2 given boxes 
   - Figure out how to create the result-box type when
     connecting 2 given boxes. 
     We may need the following operations
        - Union          : The union of the unused outputs in one box and the outputs of the other box 
        - Intersection
        - Difference
   - Could connections potentially be bidirectional? Not a single stream flowing in both directions
     but rather when doing "connect a b" some outputs of b could be routed to inputs of a and some
     outputs of a be routed to some inputs of b.
   - Is this really a pleasant way to compose components?
     - as we make connections, boxes are pulled into bigger and bigger boxes potentially with
       more and more available outputs to connect things to.
     - A "view" of the structure of what one is doing is lost
   - Reify connection "netlists" from the types    
     - Can this be done ??? 



-}  
data Box (istreams :: [*]) (ostreams :: [*]) where
  Iota :: Box '[] '[Stream l Int] 

  Dup :: Box '[Stream l a] '[Stream l1 a, Stream l2 a] 

  -- MUCH type magic is needed to get this thing to make sense 
  Connect :: Box i o -> Connection m  -> Box i1 o1 -> Box i2 o2 


myIota :: Box '[] '[Stream "iota" Int]
myIota = Iota 

splitter :: Box '[Stream l a] '[Stream l1 a,Stream l2 a]
splitter = Dup 

test1 :: Box '[] '[Stream "out1" Int, Stream "out2" Int] 
test1 = Connect myIota (undefined :: Connection '[ '("iota","in")]) splitter

