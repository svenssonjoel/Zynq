
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-} 

module Zincite.Backend.HLS.GenCpp where

import Zincite.Backend.Graph
import Zincite.Backend.BaseType
import Zincite.Syntax

import Data.Reify
import Data.List

import qualified Data.Map as M

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

pOp2 :: Op2 -> String -- TODO: Combine Op2 and Op1...
pOp2 Add = " + "
pOp2 Sub = " - "
pOp2 Mul = " * "
pOp2 Div = " / "
pOp2 Mod = " % "
-- TODO: ADD MORE 

-- TODO: May need to produce both a string and a name
--       that contains the final answer. 
pExp :: Int -> Graph ExpNode -> (String,String) -- code,result variable 
pExp identifier (Graph g res) =  snd $ pp M.empty g res 
  where ident :: Int -> String 
        ident i = "v" ++ show identifier ++ "_" ++ show i

        pp env g res =
          case M.lookup res env of
            Just v -> (env,("",v))
            Nothing -> 
              case lookup res g of
                Just node -> 
                  case node of  
                    (BinOp op n1 n2 t) ->
                      let (env1,(s1,r1)) = pp env g n1
                          (env2,(s2,r2)) = pp env1 g n2
                          var = ident res
                          env' = M.insert res var env2
                      in  (env',(pType t ++ " " ++ var ++ " = " ++ r1 ++ pOp2  op ++ r2 ++ ";\n",var))
                    -- TODO:REPEAT MANY TIMES 
                Nothing -> error "FAULTY GRAPH" 

        
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
    ident <- newIdentifier 
    let (e',res) = pExp ident e 
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


-- Create the argument list
class ReifyType a where
  reifyType :: a -> Gen ([String], Code (Graph ExpNode) )


instance ReifyType (Compute a) where
  reifyType c =
    do
      let ((a,i),code) = runCompute c 
      code' <- liftIO $ codeGraph code
      return ([], code') 

instance (BaseType a, ReifyType b) => ReifyType (StreamIn a -> b) where
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

instance (BaseType a, ReifyType b) => ReifyType (StreamOut a -> b) where
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






