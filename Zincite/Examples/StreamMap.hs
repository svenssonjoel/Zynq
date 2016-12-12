
module StreamMap where

import Zincite.Syntax
import Zincite.Backend.HLS


-- This is a stream map kernel generator. 
smap :: (Emb b, Emb a) => (a -> b) -> StreamIn a -> StreamOut b -> Compute ()
smap f in1 out =
  forever $ 
  do a <- sget in1
     sput out (f a)
   

smapAST =
  let in1 = (SIn (StreamInternal "s1" TInt))
      out = (SOut (StreamInternal "s3" TInt))
  in snd $ runCompute $ smap f in1 out
  where
    f :: Expr Int -> Expr Int 
    f a = a + 1
