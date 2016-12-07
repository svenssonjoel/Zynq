
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-} 

module Zincite.Backend.HLS.GenCpp where

import Zincite.Backend.Graph 
import Zincite.Syntax

import Data.Reify

import Control.Monad.State

------------------------------------------------------------
-- initial experiment, next step should build a new abstract
-- representation of "C++"-programs and pass that onwards
-- to CPU and FPGA backend code generators. 

include s = "#include <"++s++">\n" 

default_includes = include "hls_stream.h" 

pragma_hls_interface s = "#pragma HLS INTERFACE " ++ s ++ "\n"

pType :: Type -> String
pType TInt = "int" 

-- TODO: May need to produce both a string and a name
--       that contains the final answer. 
pExp :: Graph ExpNode -> Gen String 
pExp _ = return "EXPRESSION" 

------------------------------------------------------------
-- Goal
genCpp :: Code (Graph ExpNode) -> Gen String
genCpp Nil = return $ ""
genCpp (Declare nom t) = return $ pType t ++ " = " ++ nom ++ ";\n"
genCpp (LocalMemory (LocalMem nom size)) = return $ "unsigned char " ++ nom ++ "[" ++ show size ++  "];\n"
genCpp (Seq c1 c2) = do { c1' <- genCpp c1; c2' <-  genCpp c2; return (c1' ++ c2')}
genCpp (Assign nom t e) = return $ "aassignment"
genCpp (While name c e) = -- I dont really need the name ?? 
  do
    c' <- genCpp c
    e' <- pExp e 
    let loop = "while (" ++ e' ++"){\n" ++ c' ++ "}\n"
    return loop
genCpp (SGet targ (StreamInternal stream t0) t1) =
  return $ "STREAM GET OPERATION: " ++ targ ++ " <- " ++ stream ++ ";\n" 
  -- should be turned into a while loop that waits until data
  -- is present on the fifo. 
    
genCpp s = error $ show s 

type Gen a = StateT Int IO a 

evalGen :: Gen a -> IO a 
evalGen = flip evalStateT 0

newIdentifier :: Gen Int
newIdentifier = do {s <- get; put (s+1); return s}

------------------------------------------------------------
-- compile function
-- TODO: Generalize and Improve !


compile :: ReifyType a => a -> Gen String
compile a = 
  do (args, c) <- reifyType a
     body <-  genCpp c
     id <- newIdentifier
     let name = "f" ++ show id
         func = generateFunc name args body 
          
     -- TODO: Generate the function (using args and c) 
     return func 

generateFunc nom args body =
  "void " ++ nom ++ "(" ++ genArgs args ++ "){\n" ++ body ++ "}\n"
  where genArgs [] = ""
        genArgs [x] = x
        genArgs (x:y:xs) = x ++ "," ++ genArgs (y:xs)


-- TODO: COme up with a way to break out and generalize ReifyType. 

-- Reify base types
class ReifyBase a where
  baseType :: a -> Type

-- Repeat many times ;) 
instance ReifyBase Int where
  baseType _ = TInt


-- Create the argument list
class ReifyType a where
  reifyType :: a -> Gen ([String], Code (Graph ExpNode) )


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



-- Create instances for Compute () 
--                      StreamIn a -> Compute ()
--                      StreamOut a -> Compute ()
--                      StreamIn a -> StreamOut b -> Compute ()
--                      and so on...






