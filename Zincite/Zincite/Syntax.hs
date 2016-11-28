{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Zincite.Syntax where 

import Control.Monad.State  hiding (forever)
import Control.Monad.Writer hiding (forever)
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


type Target = Name
           
-- statment language 
data Code =
    Nil
  | MRead  Target InterfaceIO Type Exp -- DRAM Read 
  | MWrite InterfaceIO Type Exp Exp    -- DRAM Write 
  | LocalMemory LocalMem Int           -- Request use of BRAM 
  | LWrite LocalMem Type Exp Exp       -- BRAM Write
  | LRead  Target LocalMem Type Exp    -- BRAM Read
  | SGet   Target StreamInternal Type  -- Pop of a Stream 
  | SPut   StreamInternal Type Exp     -- Push onto a Stream 
  | Assign Target Type Exp             -- Assignment to variable 
  | Seq    Code Code                   -- sequencing of operations
  | While  Name Code Exp
    deriving Show 

instance Monoid Code where
  mempty = Nil
  mappend Nil b = b
  mappend a Nil = a 
  mappend a b = a `Seq` b
 
newtype Compute a = Compute (StateT Int (Writer Code) a )
  deriving (Functor, Applicative, Monad, MonadState Int, MonadWriter Code)

runCompute :: Compute a -> ((a, Int),Code) 
runCompute (Compute c) = runWriter $ runStateT c 0 
  --let ma = runStateT c 0 
  --in runWriter ma


runComputeLocal :: Compute () -> Compute Code
runComputeLocal (Compute c) =
  do
    s <- get
    let (((),i),code) = runWriter $ runStateT c s
    put i
    return code 
    


freshName :: Name -> Compute Name
freshName pre = do
  s <- get
  put (s + 1)
  return (pre ++ show s) 

  
class MemoryIO a where
  mread  :: InterfaceIO -> Expr Address -> Compute (Expr a) 
  mwrite :: InterfaceIO -> Expr Address -> Expr a -> Compute ()

instance MemoryIO Int where
  mread = undefined
  mwrite = undefined


-------------------------------------------------------------
-- Expable class (called Emb for now) 
class Emb a where
  typeOf :: a -> Type
  toExp  :: a -> Exp
  fromExp :: Exp -> a 

instance Emb (Expr Int) where
  typeOf _ = TInt
  toExp = unE
  fromExp = E 

instance Emb (Expr Bool) where
  typeOf _ = TBool
  toExp = unE
  fromExp = E 


------------------------------------------------------------
-- Stream get and put 
sget :: StreamIn a -> Compute (Expr a)
sget (SIn si@(StreamInternal _ typ)) =
  do
    nom <- freshName "s"
    let var = E $ Variable nom
    
    tell $ SGet nom si typ
    return var

sput :: StreamOut a -> Expr a -> Compute ()
sput (SOut si@(StreamInternal _ typ)) (E e) = tell $ SPut si typ e 

------------------------------------------------------------
-- Allocate a statically known quantity of local memory.
-- In other words request use request the use of some amount of BRAM
bram :: Int -> Compute LocalMem
bram = undefined

------------------------------------------------------------
-- while
while :: (Emb (a)) => (a -> Expr Bool) -> (a -> Compute ()) -> a -> Compute ()
while cond body init =
  do
    nom <- freshName "tmp" 
    let var = Variable $ nom
        bodyComp = body (fromExp var)
        condExpr = cond (fromExp var)
    bodyCode <- runComputeLocal bodyComp
    tell $ Assign nom (typeOf init) (toExp init)      -- HACK 
    tell $ While nom bodyCode (unE condExpr)  

------------------------------------------------------------
-- Loop a computation forever 
forever :: Compute () -> Compute ()
forever c = while (id) (\_ -> c) true

------------------------------------------------------------
-- test

test1 :: StreamIn Int -> StreamIn Int -> StreamOut Int -> Compute ()
test1 ins1 ins2 os =
  forever $ 
  do 
    a <- sget ins1 
    b <- sget ins2
    sput os (a + b)  

aTest1 = runCompute $ test1 (SIn (StreamInternal "s1" TInt)) (SIn (StreamInternal "s2" TInt)) (SOut (StreamInternal "s3" TInt))



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
