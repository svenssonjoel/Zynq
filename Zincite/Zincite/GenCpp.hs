
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Zincite.GenCpp where

import Zincite.Syntax

import Data.Reify

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


------------------------------------------------------------
-- Goal
genCpp :: Code (Graph ExpNode) -> String
genCpp = undefined



------------------------------------------------------------
-- Class Compile (move later)

class Compile a where
  compile :: a -> String

-- Create instances for Compute () 
--                      StreamIn a -> Compute ()
--                      StreamOut a -> Compute ()
--                      StreamIn a -> StreamOut b -> Compute ()
--                      and so on...






