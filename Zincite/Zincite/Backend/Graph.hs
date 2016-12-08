{-# LANGUAGE TypeFamilies #-}

module Zincite.Backend.Graph where

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
    MWrite m e1 e2 t -> 
      do
        e1' <- expToGraph e1
        e2' <- expToGraph e2
        return $ MWrite m e1' e2' t
    LocalMemory m -> return $ LocalMemory m
    SGet targ s t -> return $ SGet targ s t
    SPut s e t ->
      do
        e' <- expToGraph e 
        return $ SPut s e' t
    Assign targ e t ->
      do
        e' <- expToGraph e
        return $ Assign targ e' t
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


