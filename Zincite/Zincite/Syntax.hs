{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zincite.Syntax where 

import Control.Monad.State  hiding (forever)
import Control.Monad.Writer hiding (forever)
import Control.Applicative

import Data.Word 

type Name = String 
type Address = Word
type Size = Int
data Data

--data InterfaceIO = InterfaceIO Name deriving Show 
data StreamInternal  = StreamInternal Name Type deriving Show 
newtype StreamIn a = SIn StreamInternal deriving Show -- Phantom type a 
newtype StreamOut a = SOut StreamInternal deriving Show 
--data LocalMem = LocalMem Name Int deriving Show


-- Unify the memory types
data MemoryInternal = InterfaceIO Name
                    | LocalMem Name Int
                    deriving Show 

data Local
data Global 
newtype Memory a = M {unM :: MemoryInternal}
  deriving Show 


  
data Type = TInt | TUInt | TFloat | TBool | TStream Type | TTuple [Type]
  deriving Show 

data Value = IntVal Int
           | UIntVal Word 
           | FloatVal Float
           | BoolVal Bool 
             deriving Show
                      
-- expression language
-- TODO: Support for Tuples 
data Exp =
  Constant Value
  | Variable Name 
  | BinOp Op2 Exp Exp
  | UnOp  Op1 Exp 
  | MRead MemoryInternal Type Exp -- Read from Interface at address
  | Tuple [(Exp,Type)] -- Internal representation of tuples
 -- | LRead LocalMem Type Exp    -- Read from local memory at address
    deriving Show 

data Op2 =
  Add | Sub | Mul | Div | Mod
  | BitAnd | BitOr | BitXor
  deriving Show 
data Op1 =
  Neg | Not
  | Min | Max -- Expects argument to be a tuple 
  deriving Show 


newtype Expr a = E {unE :: Exp}

------------------------------------------------------------
-- Arithmetic and operations


instance Num (Expr Int) where
  (+) a b = E $ BinOp Add (unE a) (unE b)
  (-) a b = E $ BinOp Sub (unE a) (unE b)
  (*) a b = E $ BinOp Mul (unE a) (unE b)
  abs = undefined
  signum = undefined
  fromInteger i = E $ Constant $ IntVal $ fromInteger i

instance Num (Expr Word) where
  (+) a b = E $ BinOp Add (unE a) (unE b)
  (-) a b = E $ BinOp Sub (unE a) (unE b)
  (*) a b = E $ BinOp Mul (unE a) (unE b)
  abs = undefined
  signum = undefined
  fromInteger i = E $ Constant $ UIntVal $ fromInteger i

-- TODO: Correct constraints on "a" for these functions 
mod :: Emb a => a -> a -> a 
mod a b = fromExp $ BinOp Mod (toExp a) (toExp b) 

div :: Emb a => a -> a -> a 
div a b = fromExp $ BinOp Div (toExp a) (toExp b) 

min :: Emb a =>  (a, a) -> a
min tup = fromExp $ UnOp Min (toExp tup)

max :: Emb a => (a, a) -> a 
max tup = fromExp  $ UnOp Max (toExp tup) 

------------------------------------------------------------
-- Booleans and bool ops 
true = E $ Constant $ BoolVal True 
false = E $ Constant $ BoolVal False


type Target = Name
           
-- statment language
-- TODO: Consider making MRead, LRead, SGet (and maybe others part of the expression lang) 
data Code =
    Nil
  | Declare Name Type                  -- Variable declaration
 -- | MRead  Target InterfaceIO Type Exp -- DRAM Read 
  | MWrite MemoryInternal Type Exp Exp    -- DRAM Write 
  | LocalMemory MemoryInternal               -- Request use of BRAM 
--  | LWrite MemoryInternal Type Exp Exp       -- BRAM Write
--  | LRead  Target LocalMem Type Exp    -- BRAM Read
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



-- TODO: Clean up things 
class MemoryIO a where
  mread  :: Memory m -> Expr Address -> Expr a 
  mwrite :: Memory m -> Expr Address -> Expr a -> Compute ()

instance MemoryIO Int where
  mread interface addr =
    E $ MRead (unM interface) TInt (unE addr) 
    
  mwrite interface addr value =
    tell $ MWrite (unM interface) TInt (unE addr) (unE value) 

instance MemoryIO Float where
  mread interface addr =
    E $ MRead (unM interface) TFloat (unE addr) 
    
  mwrite interface addr value =
    tell $ MWrite (unM interface) TFloat (unE addr) (unE value) 


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

instance Emb (Expr Word) where
  typeOf _ = TUInt
  toExp = unE
  fromExp = E 


instance Emb (Expr Bool) where
  typeOf _ = TBool
  toExp = unE
  fromExp = E

instance (Emb a, Emb b) => Emb (a,b) where
  typeOf (a,b) = TTuple [typeOf a, typeOf b]
  toExp  (a,b) = Tuple [(toExp a, typeOf a), (toExp b, typeOf b)]
  fromExp (Tuple [(e1,t1),(e2,t2)]) = (fromExp e1, fromExp e2) 

instance (Emb a, Emb b, Emb c) => Emb (a,b,c) where
  typeOf (a,b,c) = TTuple [typeOf a, typeOf b,typeOf c]
  toExp  (a,b,c) = Tuple [(toExp a, typeOf a), (toExp b, typeOf b), (toExp c, typeOf c)]
  fromExp (Tuple [(e1,t1),(e2,t2), (e3,t3)]) = (fromExp e1, fromExp e2, fromExp e3) 
  


------------------------------------------------------------
-- Declare a variable (scope is as expected by do block hierarchy) 
declare :: forall a . Emb a => Compute (a)  -- SCOPED TYPE VARIABLE 
declare =
  do nom <- freshName "v"
     tell $ Declare nom (typeOf (undefined :: a)) -- SCOPED TYPE VARIABLE enables this
     return $ fromExp (Variable nom) 

infixl 1 =: 
(=:) :: forall a . Emb a => a -> a -> Compute ()
(=:) left right =
  case (toExp left,toExp right) of
    (Variable nom, anything) -> tell $ Assign nom (typeOf right) anything 
    (_,_) -> error "assign to non-variable" 
    

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
bram :: Int -> Compute (Memory Local) 
bram n =
  do nom <- freshName "bram"
     let lmem = LocalMem nom n
     tell $ LocalMemory lmem
     return $ M lmem

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
