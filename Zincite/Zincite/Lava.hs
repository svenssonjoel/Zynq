


-- Lava-like operations for use on lists of expressions 
module Zincite.Lava where


import Zincite.Syntax




------------------------------------------------------------
-- Convert a consequtive words of memory to a list of memory accesses
listFromMem :: MemoryIO a => Memory m -> Expr Address -> Int -> [Expr a]
listFromMem mem addr n =
  [ mread mem (fromIntegral ix)|  ix <- [0..(n-1)]] 




------------------------------------------------------------
--

evens :: ([a] -> [a]) -> [a] -> [a] 
evens f []       = []
evens f (a:b:cs) = f [a,b] ++ evens f cs 


------------------------------------------------------------
--

parl :: ([a] -> [b]) -> ([a] -> [b]) -> [a] -> [b]
parl _ _ []   = error "parl: input list too short"
parl _ _ [a]  = error "parl: inpup list too short"
parl f g xs   = f (take nhalf xs) ++ g (drop nhalf xs) 
  where n = length xs
        nhalf = n `div` 2 

------------------------------------------------------------
--

two :: ([a] -> [b]) -> [a] -> [b]
two f = parl f f 
