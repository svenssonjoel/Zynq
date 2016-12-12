

module StreamAdd where

import Zincite.Syntax
import Zincite.Backend.HLS

addStreams :: StreamIn (ZInt) -> StreamIn (ZInt) -> StreamOut (ZInt) -> Compute ()
addStreams in1 in2 out =
  forever $ 
  do a <- sget in1
     b <- sget in2
     sput out (a + b)
   
addStreamsAST =
  let in1 = (SIn (StreamInternal "s1" TInt))
      in2 = (SIn (StreamInternal "s2" TInt))
      out = (SOut (StreamInternal "s3" TInt))
  in snd $ runCompute $ addStreams in1 in2 out   

