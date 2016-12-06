
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-} 

module Zincite.GenCpp where

import Zincite.Syntax

import Data.Reify

import Control.Monad.State

------------------------------------------------------------
-- Reify graph
instance MuRef Exp where
  type DeRef Exp = ExpNode
  mapDeRef f (Exp s) = traverse f s 

-- Code Exp to Code Graph conversion
-- as a first step.

codeGraph :: Code Exp -> IO (Code (Graph ExpNode))
codeGraph c =
  case c of
    Nil -> return Nil
    Declare nom t -> return $ Declare nom t
    MWrite m t e1 e2 -> 
      do
        e1' <- expToGraph e1
        e2' <- expToGraph e2
        return $ MWrite m t e1' e2'
    LocalMemory m -> return $ LocalMemory m
    SGet targ s t -> return $ SGet targ s t
    SPut s t e ->
      do
        e' <- expToGraph e 
        return $ SPut s t e'
    Assign targ t e ->
      do
        e' <- expToGraph e
        return $ Assign targ t e'
    Seq c1 c2 ->
      do
       c1' <- codeGraph c1
       c2' <- codeGraph c2
       return $ Seq c1' c2'
    While nom c e ->
      do
        c' <- codeGraph c
        e' <- expToGraph e
        return $ While nom c' e' 
          
        
 -- Traverse the Code and generate graph for each expression


expToGraph :: MuRef s => s -> IO (Graph (DeRef s))
expToGraph = reifyGraph 



------------------------------------------------------------
-- initial experiment, next step should build a new abstract
-- representation of "C++"-programs and pass that onwards
-- to CPU and FPGA backend code generators. 

include s = "#include <"++s++">\n" 

default_includes = include "hls_stream.h" 

pragma_hls_interface s = "#pragma HLS INTERFACE " ++ s ++ "\n"

pType :: Type -> String
pType TInt = "int" 


------------------------------------------------------------
-- Goal
genCpp :: Code (Graph ExpNode) -> Gen String
genCpp = undefined

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
     id <- newIdentifier
     let name = "f" ++ show id
         func = generateFunc name args "" 
     -- TODO: Generate the function (using args and c) 
     return func 

generateFunc nom args body =
  "void " ++ nom ++ "(" ++ genArgs args ++ "){\n" ++ body ++ "}\n"
  where genArgs [] = ""
        genArgs [x] = x
        genArgs (x:y:xs) = x ++ "," ++ genArgs (y:xs)

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






