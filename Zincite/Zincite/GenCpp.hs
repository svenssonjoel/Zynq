
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Zincite.GenCpp where

import Zincite.Syntax


-- TODO: More principled approach
-- TODO: To generate memory reads we need type information 



pexp :: Exp -> String
pexp (Constant (IntVal i)) = show i
pexp (Constant (BoolVal True)) = "1";
pexp (Constant (BoolVal False)) = "0";
pexp (Constant (FloatVal f)) = show f
pexp (BinOp op e1 e2) = "(" ++ pexp e1 ++ binop op ++ pexp e2 ++ ")"
pexp (UnOp  op e1)    = "(" ++ unop op ++ pexp e1 ++ ")"  


binop Add = " + "
binop Sub = " - "
binop Mul = " * "
binop Div = " / "

unop neg = " - " 
