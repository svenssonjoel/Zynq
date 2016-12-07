

module Zincite.Backend.ReifyType where

import Zincite.Syntax


-- Reify base types
class ReifyBase a where
  baseType :: a -> Type


-- Repeat many times ;) 
instance ReifyBase Int where
  baseType _ = TInt


-- Create the argument list
class ReifyType a where
  reifyType :: a -> Gen ([(Type,String)], Code (Graph ExpNode) )


instance ReifyType (Compute a) where
  reifyType c =
    do
      let ((a,i),code) = runCompute c 
      code' <- liftIO $ codeGraph code
      return ([], code') 

instance (ReifyBase a, ReifyType b) => ReifyType (StreamIn a -> b) where
  reifyType f =
    do
      i <- newIdentifier
      let v_nom = "instream" ++ show i 
          s_in = SIn (StreamInternal v_nom t) 
          rest = f s_in
          cppArg = "stream<"++ pType t ++ "> &"++v_nom
      (vars,c) <- reifyType rest
      return $ (cppArg  : vars, c)  
    where t = baseType (undefined :: a) 

instance (ReifyBase a, ReifyType b) => ReifyType (StreamOut a -> b) where
  reifyType f =
    do
      i <- newIdentifier
      let v_nom = "outstream" ++ show i 
          s_in = SOut (StreamInternal v_nom t) 
          rest = f s_in
          cppArg = "stream<"++ pType t ++ "> &"++v_nom
      (vars,c) <- reifyType rest
      return $ (cppArg : vars, c) 
    where t = baseType (undefined :: a) 


