{-# LANGUAGE GADTs, TypeOperators, DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-} 
-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Zincite.SymbolTest where 

import GHC.TypeLits

import Data.Proxy

import Data.Type.List 
import Data.Singletons

-- Empty dummy stream 
data Stream (l :: Symbol) a

data Connection (c :: [(Symbol, Symbol)])

{- 
  TODO
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

type family GetLabel a 
type instance GetLabel (Stream l t) = Proxy l 

-- Something is very wrong here .. 
-- I wanted to write this: (TyFun (Stream l t) l -> *) 
-- But got errors i could not understand. 
-- data GetLabel' :: TyFun (Stream l t) l -> * where 
data GetLabel' :: TyFun a b -> * where 
     GetLabel' :: GetLabel' f
 
type instance Apply GetLabel' a = GetLabel a
-- exists in Data.Type.List (requires curried type fun "TyFun a b") 
--type family Map (f :: k -> k) (l :: [k]) :: [k]
--type instance Map f '[] = '[] 
--type instance Map f (x ': xs) = f x ': (Map f xs) 
  
-- Reify a type-list of Symbols into a value-list of Strings 
class StringSymbols (a :: [k]) where 
  stringSymbols :: Proxy a -> [String] 
  
instance StringSymbols '[] where 
  stringSymbols _ = []

instance (StringSymbols xs, KnownSymbol x) => StringSymbols ((Proxy x) ': xs) where 
  stringSymbols _ = 
    symbolVal (undefined :: Proxy x) 
      : (stringSymbols (undefined :: Proxy xs)) 
  
--Does not quite work, of course. Need some way to make sure that all 
--the symbols in the lists always have the property "KnownSymbol" 
-- If that is the problem: 
--
-- 
-- 

 
-- testing: grabbing a symbol  
testGetASymbol :: forall i o. StringSymbols (Map GetLabel' o) =>  Box i o -> [String] 
testGetASymbol _ = stringSymbols (undefined :: Proxy (Map GetLabel' o)) 
-- Example: 
-- > testGetASymbol myIota
-- ["iota"]

-- Example: 
-- > testGetASymbol test1  
--   * No instance for (StringSymbols
--                        '[Zincite.SymbolTest.Stream "out2" Int])
--       arising from a use of `testGetASymbol'
--   * In the expression: testGetASymbol test1
--     In an equation for `it': it = testGetASymbol test1
--
-- NOTE: I am guessing this is because of the KnownSymbol property not being omnipresent. 
--   


