{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative 

type Name = String 
type Address = Int
type Size = Int
data Data

data InterfaceIO = InterfaceIO Name deriving Show 
data StreamInternal  = StreamInternal Name Type deriving Show 
newtype StreamIn a = SIn StreamInternal deriving Show -- Phantom type a 
newtype StreamOut a = SOut StreamInternal deriving Show 
data LocalMem = LocalMem Name deriving Show 
  
data Type = TInt | TFloat | TBool | TStream Type
  deriving Show 

data Value = IntVal Int
           | FloatVal Float
           | BoolVal Bool 
             deriving Show
                      
-- expression language
data Exp =
  Constant Value
  | Variable Name 
  | BinOp Op2 Exp Exp
  | UnOp  Op1 Exp Exp
    deriving Show 

newtype Expr a = E {unE :: Exp}

instance Num (Expr Int) where
  (+) a b = E $ BinOp Add (unE a) (unE b)
  (-) a b = E $ BinOp Sub (unE a) (unE b)
  (*) a b = E $ BinOp Mul (unE a) (unE b)
  abs = undefined
  signum = undefined
  fromInteger i = E $ Constant $ IntVal $ fromInteger i


true = E $ Constant $ BoolVal True 
false = E $ Constant $ BoolVal False

data Op2 = Add | Sub | Mul | Div | BitAnd | BitOr | BitXor
  deriving Show 
data Op1 = Neg | Not 
  deriving Show 
  
-- statment language 
data Code =
    Nil
  | MRead  InterfaceIO Type Exp     -- DRAM Read 
  | MWrite InterfaceIO Type Exp Exp -- DRAM Write 
  | LocalMemory LocalMem Int        -- Request use of BRAM 
  | LWrite Name Type Exp Exp        -- BRAM Write
  | LRead  Name Type Exp            -- BRAM Read
  | SGet   StreamInternal Type      -- Pop of a Stream 
  | SPut   StreamInternal Type      -- Push onto a Stream 
  | Assign String Type Exp          -- Assignment to variable 
  | Seq    Code Code                -- sequencing of operations
  | While  Name Code Exp
    deriving Show 

instance Monoid Code where
  mempty = Nil
  mappend a b = a `Seq` b
 
newtype Compute a = Compute (StateT Int (Writer Code) a )
  deriving (Functor, Applicative, Monad, MonadState Int, MonadWriter Code)

runCompute :: Compute a -> ((a, Int),Code) 
runCompute (Compute c) = runWriter $ runStateT c 0 
  --let ma = runStateT c 0 
  --in runWriter ma
           
class MemoryIO a where
  mread  :: InterfaceIO -> Expr Address -> Compute (Expr a) 
  mwrite :: InterfaceIO -> Expr Address -> Expr a -> Compute ()

instance MemoryIO Int where
  mread = undefined
  mwrite = undefined 


------------------------------------------------------------
-- Stream get and put 
sget :: StreamIn a -> Compute (Expr a)
sget = undefined

sput :: StreamOut a -> Expr a -> Compute ()
sput = undefined

------------------------------------------------------------
-- Allocate a statically known quantity of local memory.
-- In other words request use request the use of some amount of BRAM
bram :: Int -> Compute LocalMem
bram = undefined

------------------------------------------------------------
-- while
while :: (Expr a -> Expr Bool) -> (Expr a -> Compute ()) -> Expr a -> Compute ()
while cond body init =
  do
    s <- get -- get the counter
    
    let nom = "tmp" ++ (show s)
        var = E $ Variable $ nom
        bodyComp = body var
        condExpr = cond var
        (((),i),bodyCode) = runCompute bodyComp
    tell $ Assign nom TBool (unE init)      -- HACK 
    tell $ While nom bodyCode (unE condExpr)  

------------------------------------------------------------
-- Loop a computation forever 
forever :: Compute () -> Compute ()
forever c = undefined

test1 :: StreamIn Int -> StreamIn Int -> StreamOut Int -> Compute ()
test1 ins1 ins2 os = do
  a <- sget ins1 
  b <- sget ins2
  sput os (a + b)  
  



-- replace this with a type level natural number perhaps
-- data None 
-- data (:+:) a b = a :+: b

  
-- TODO: Allow for grouping of mem interfaces using interconnects
---      
-- data Block mem instreams outstreams where
--   ComputeBlock :: (mem -> StreamIn instreams -> StreamOut outstreams -> Comp ()) -> Block mem instreams outstreams 
--   Seq :: Block m1 i1 o1 -> Block m2 o1 o3 -> Block (m1 :+: m2) i1 o3 


--   -- Constraint m1 > 0 
--   MemIC :: Block m1 i1 o1 -> Block () i1 o1 -- replace () with the type level natural 1
--   FIFO  :: 
