{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import System.IO
import Data.Word
import Data.Int

import Text.Show.Functions

data Dimensions = I Size | II Size Size | III Size Size Size
  deriving Show 

type Tag = Integer   -- Meta information for code generator  
type Size = Integer   -- Later Some expression type


data Type = TInt32
  deriving Show 

-- Values
data Value = VInt32 Int
  deriving Show 

-- Expression level language 
data ExpNode s =  
  Const  Value 
  | Operator Op [s] Type
  deriving Show

data Expr = Expr (ExpNode Expr)
  deriving Show 

data Op = Add | Sub | Mul | Div
  deriving Show

-- Array Computation Components
data Z s =  
  ConstArray Expr Dimensions
  | Map (Expr -> Expr) s
  deriving (Functor, Foldable, Traversable, Show)


-- Phantom type wrapper
newtype Exp a = E {unE :: Expr}
  deriving Show

-- Helpers
operator :: Op -> [Exp a] -> Type -> Exp a
operator op elist ret_t = E $ Expr $ Operator op (map unE elist) ret_t


-- Standard instances 
instance Num (Exp Int32) where
  (+) a b = operator Add [a,b] TInt32
  (-) a b = operator Sub [a,b] TInt32
  (*) a b = operator Mul [a,b] TInt32
  abs = error "Not implemented"
  signum = error "not implemented"
  fromInteger a = E $ Expr $ Const $ VInt32 $ fromInteger a
