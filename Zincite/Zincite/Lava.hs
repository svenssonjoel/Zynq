


-- Lava-like operations for use on lists of expressions 
module Zincite.Lava where


import Zincite.Syntax hiding (div) 

import Prelude hiding (min,max) 


------------------------------------------------------------
-- Convert a consequtive words of memory to a list of memory accesses
listFromMem :: Emb a => Memory m -> Expr Address -> Int -> [a]
listFromMem mem addr n =
  [ mread mem (fromIntegral ix)|  ix <- [0..(n-1)]] 


------------------------------------------------------------
--
(>->) = flip (.) 

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


------------------------------------------------------------
--

unpair :: [(a,a)] -> [a]
unpair [] = []
unpair ((x,y):xs) = x : y : unpair xs 

pair :: [a] -> [(a,a)]
pair [] = []
pair [a] = error "pair: input list is too short" -- really check for even length
pair (x:y:xs) = (x,y) : pair xs


halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = (pre,post) 
  where n = length xs
        nhalf = n `div` 2
        pre = take nhalf xs
        post = drop nhalf xs

------------------------------------------------------------
--

riffle :: [a] -> [a]
riffle [] = []
riffle [x] = [x] 
riffle xs = unpair $ zip pre post  
  where
    (pre,post) = halve xs 

unriffle [] = []
unriffle [x] = [x] 
unriffle xs = let (pre,post) = unzip (pair xs)
              in pre ++ post 


------------------------------------------------------------
--

ilv :: ([a] -> [b]) -> [a] -> [b]
ilv f = riffle . two f . unriffle 


------------------------------------------------------------
-- butterfly
bfly :: (Eq a, Num a) => a -> ([a1] -> [a1]) -> [a1] -> [a1]
bfly 1 f = f
bfly n f = ilv (bfly (n-1) f) >-> evens f  


------------------------------------------------------------
-- two sorter
twosort :: Emb a =>  [a] -> [a]
twosort xs = unpair (map minmax (pair xs)) -- check lengths and signal error
  where minmax p = (min p, max p)  


------------------------------------------------------------
-- Batchers
sortB :: (Eq a, Num a) => a -> ([b] -> [b]) -> [b] -> [b]
sortB 0 cmp = id
sortB n cmp = parl (sortB k cmp) (sortB k cmp >-> reverse) >-> bfly n cmp  
  where k = n - 1
