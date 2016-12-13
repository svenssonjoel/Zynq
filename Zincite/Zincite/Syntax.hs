{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

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
newtype StreamIn a = SIn {unsin :: StreamInternal} deriving Show -- Phantom type a 
newtype StreamOut a = SOut {unsout :: StreamInternal} deriving Show 
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
data ExpNode s =
  Constant Value 
  | Variable Name Type
  | BinOp Op s s Type
  | UnOp  Op s Type
  | CFunc Name [(s,Type)] -- EASY way out for Min max and other C functions we may want to call
  | MRead MemoryInternal s Type -- Read from Interface at address
  | Tuple [(s,Type)] -- Internal representation of tuples
 -- | LRead LocalMem Type Exp    -- Read from local memory at address
  deriving (Functor, Foldable, Traversable, Show)

constantE :: Value -> Exp
constantE v = Exp $ Constant v

variableE :: Name -> Type -> Exp
variableE nom t = Exp $ Variable nom t 

binOpE :: Op -> Exp -> Exp -> Type -> Exp
binOpE op e1 e2 t = Exp $ BinOp op e1 e2 t 

unOpE :: Op -> Exp -> Type -> Exp
unOpE op e1 t = Exp $ UnOp op e1 t

mreadE :: MemoryInternal -> Exp -> Type -> Exp
mreadE m e t = Exp $ MRead m e t

tupleE :: [(Exp,Type)] -> Exp
tupleE ls = Exp $ Tuple ls 

data Exp = Exp {expNode :: ExpNode Exp}
  deriving Show

data Op =
  Add | Sub | Mul | Div | Mod
  | BitAnd | BitOr | BitXor
  | Neg | Not
  | Min | Max -- Expects argument to be a tuple  
  deriving Show 

newtype Expr a = E {unE :: Exp}

unExpr (E (Exp e)) = e 

expr :: ExpNode Exp -> Expr a
expr e = E $ Exp e 

------------------------------------------------------------
-- Arithmetic and operations


instance Num (Expr Int) where
  (+) a b = fromExp $ binOpE Add (unE a) (unE b) TInt 
  (-) a b = fromExp $ binOpE Sub (unE a) (unE b) TInt 
  (*) a b = fromExp $ binOpE Mul (unE a) (unE b) TInt
  abs = undefined
  signum = undefined
  fromInteger i = fromExp $ constantE $ IntVal $ fromInteger i

instance Num (Expr Word) where
  (+) a b = fromExp $ binOpE Add (unE a) (unE b) TUInt
  (-) a b = fromExp $ binOpE Sub (unE a) (unE b) TUInt 
  (*) a b = fromExp $ binOpE Mul (unE a) (unE b) TUInt
  abs = undefined
  signum = undefined
  fromInteger i = fromExp $ constantE $ UIntVal $ fromInteger i

-- TODO: Correct constraints on "a" for these functions 
mod :: Emb a => a -> a -> a 
mod a b = fromExp $ binOpE Mod (toExp a) (toExp b) (typeOf a) 

div :: Emb a => a -> a -> a 
div a b = fromExp $ binOpE Div (toExp a) (toExp b) (typeOf a) 

min :: forall a. Emb a =>  (a, a) -> a
min tup = fromExp $ unOpE Min (toExp tup) (typeOf (undefined :: a))

max :: forall a. Emb a => (a, a) -> a 
max tup = fromExp  $ unOpE Max (toExp tup) (typeOf (undefined :: a))  

------------------------------------------------------------
-- Booleans and bool ops

true :: Expr Bool 
true = fromExp $ constantE $ BoolVal True
false :: Expr Bool 
false = fromExp $ constantE $ BoolVal False


type Target = Name
           
-- statment language
-- TODO: Consider making MRead, LRead, SGet (and maybe others part of the expression lang) 
data Code e =
    Nil
  | Declare Name Type                  -- Variable declaration
 -- | MRead  Target InterfaceIO Type Exp -- DRAM Read 
  | MWrite MemoryInternal  e e Type   -- DRAM Write 
  | LocalMemory MemoryInternal               -- Request use of BRAM 
--  | LWrite MemoryInternal Type Exp Exp       -- BRAM Write
--  | LRead  Target LocalMem Type Exp    -- BRAM Read
  | SGet   Target StreamInternal Type  -- Pop of a Stream 
  | SPut   StreamInternal e Type     -- Push onto a Stream 
  | Assign Target  e Type            -- Assignment to variable 
  | Seq    (Code e) (Code e)          -- sequencing of operation
  | While  Name (Code e) e           -- Name bodyCode cond
    deriving Show 

instance Monoid (Code e) where
  mempty = Nil
  mappend Nil b = b
  mappend a Nil = a 
  mappend a b = a `Seq` b
 
newtype Compute a = Compute (StateT Int (Writer (Code Exp) ) a )
  deriving (Functor, Applicative, Monad, MonadState Int, MonadWriter (Code (Exp)))

type CodeE = Code Exp            

runCompute :: Compute a -> ((a, Int),CodeE) 
runCompute (Compute c) = runWriter $ runStateT c 0 

runComputeLocal :: Compute () -> Compute CodeE
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
-- class MemoryIO a where
--   mread  :: Memory m -> Expr Address -> Expr a 
--   mwrite :: Memory m -> Expr Address -> Expr a -> Compute ()

-- instance MemoryIO Int where
--   mread interface addr =
--     fromExp $ mreadE (unM interface) (unE addr) TInt 
    
--   mwrite interface addr value =
--     tell $ MWrite (unM interface) (unE addr) (unE value) TInt 

-- instance MemoryIO Float where
--   mread interface addr =
--     fromExp $ mreadE (unM interface) (unE addr) TFloat 
    
--   mwrite interface addr value =
--     tell $ MWrite (unM interface) (unE addr) (unE value) TFloat

mread  :: forall a m. Emb a => Memory m -> Expr Address -> a 
mread interface addr =
  fromExp $ mreadE (unM interface) (unE addr) (typeOf (undefined :: a))

mwrite :: forall a m. Emb a => Memory m -> Expr Address -> a -> Compute ()  
mwrite interface addr value =
  tell $ MWrite (unM interface) (unE addr) (toExp value) (typeOf (undefined :: a))


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

instance Emb (Expr Float) where
  typeOf _ = TFloat
  toExp = unE
  fromExp = E 


instance Emb (Expr Bool) where
  typeOf _ = TBool
  toExp = unE
  fromExp = E

instance (Emb a, Emb b) => Emb (a,b) where
  typeOf (a,b) = TTuple [typeOf a, typeOf b]
  toExp  (a,b) = tupleE [(toExp a, typeOf a), (toExp b, typeOf b)]
  fromExp (Exp (Tuple [(e1,t1),(e2,t2)])) = (fromExp e1, fromExp e2) 

instance (Emb a, Emb b, Emb c) => Emb (a,b,c) where
  typeOf (a,b,c) = TTuple [typeOf a, typeOf b,typeOf c]
  toExp  (a,b,c) = tupleE [(toExp a, typeOf a), (toExp b, typeOf b), (toExp c, typeOf c)]
  fromExp (Exp (Tuple [(e1,t1),(e2,t2), (e3,t3)])) = (fromExp e1, fromExp e2, fromExp e3) 


-- Class of Zincite values
type Z a = Emb a 

-- Shorthand for Z(incite) types (Emb a => Expr a)
type ZInt = Expr Int
type ZWord = Expr Word
type ZAddress = Expr Word
type ZFloat = Expr Float
type ZBool = Expr Bool
-- .. TODO: Repeat many times.


------------------------------------------------------------
-- Declare a variable (scope is as expected by do block hierarchy) 
declare :: forall a . Emb a => Compute (a)  -- SCOPED TYPE VARIABLE 
declare =
  do nom <- freshName "v"
     tell $ Declare nom (typeOf (undefined :: a)) -- SCOPED TYPE VARIABLE enables this
     return $ fromExp (variableE nom (typeOf ( undefined :: a)))

-- TODO: Figure out what binding "force" makes sense here 
infixl 1 =: 
(=:) :: forall a . Emb a => a -> a -> Compute ()
(=:) left right =
  case (toExp left,toExp right) of 
    (Exp (Variable nom t), anything) -> tell $ Assign nom anything (typeOf right) 
    (_,_) -> error "assign to non-variable" 
    


------------------------------------------------------------
-- DONE: Maybe we should expect the "a" after StreamIn, StreamOut
--       to be Emb a.. (Expr something) 
------------------------------------------------------------
-- Stream get and put 
sget :: Emb a => StreamIn a -> Compute a
sget (SIn si@(StreamInternal _ typ)) =
  do
    nom <- freshName "s"
    let var = fromExp $ variableE nom typ
    
    tell $ SGet nom si typ
    return var

sput :: Emb a => StreamOut a -> a -> Compute ()
sput (SOut si@(StreamInternal _ typ)) e = tell $ SPut si (toExp e) typ

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
while :: (Emb a) => (a -> Expr Bool) -> (a -> Compute ()) -> a -> Compute ()
while cond body init =
  do
    nom <- freshName "tmp" 
    let var  = fromExp $ variableE nom (typeOf init) -- same type as init 
        bodyComp = body var
        condExpr = cond var
    bodyCode <- runComputeLocal bodyComp
    tell $ Assign nom (toExp init) (typeOf init)   
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

data None
data (:+:) a b = a :+: b

-- The Block language differentiates between input and output streams
-- byt their position in the argument list. 
data Stream a = S StreamInternal 

-- The "Compute" language differentiates between input and output streams
-- by them having different types. Thus a conversion mechanism is needed.

class StreamInputs a where
  type I a
  streamInputs :: a -> [Int] -> (I a)

class StreamOutputs a where
  type O a 
  streamOutputs :: a -> [Int] -> (O a)

-- TODO: Come up with a way to make a recursive implementation of this
--       possible (if at all possible to begin with) 
instance Emb a => StreamInputs (Stream a) where
  type I (Stream a) = StreamIn a
  streamInputs s (i:_) = SIn (StreamInternal ("input"++show i) (typeOf  (undefined :: a)))

instance (Emb a, Emb b) => StreamInputs (Stream a, Stream b) where
  type  I (Stream a, Stream b) = (StreamIn a, StreamIn b)
  streamInputs s (i:j:_) =
    (SIn (StreamInternal ("input"++show i) (typeOf (undefined :: a)))
      , SIn (StreamInternal ("input"++show j) (typeOf (undefined :: b))))

instance (Emb a, Emb b, Emb c) => StreamInputs (Stream a, Stream b, Stream c) where
  type  I (Stream a, Stream b, Stream c) = (StreamIn a, StreamIn b, StreamIn c)
  streamInputs s (i:j:k:_) =
    (SIn (StreamInternal ("input"++show i) (typeOf (undefined :: a)))
      , SIn (StreamInternal ("input"++show j) (typeOf (undefined :: b)))
      , SIn (StreamInternal ("input"++show k) (typeOf (undefined :: c))))

-- A few Outputs instances 
instance Emb a => StreamOutputs (Stream a) where
  type O (Stream a) = StreamOut a
  streamOutputs s (i:_) = SOut (StreamInternal ("output"++show i) (typeOf  (undefined :: a)))

instance (Emb a, Emb b) => StreamOutputs (Stream a, Stream b) where
  type  O (Stream a, Stream b) = (StreamOut a, StreamOut b)
  streamOutputs s (i:j:_) =
    (SOut (StreamInternal ("output"++show i) (typeOf (undefined :: a)))
      , SOut (StreamInternal ("output"++show j) (typeOf (undefined :: b))))

instance (Emb a, Emb b, Emb c) => StreamOutputs (Stream a, Stream b, Stream c) where
  type  O (Stream a, Stream b, Stream c) = (StreamOut a, StreamOut b, StreamOut c)
  streamOutputs s (i:j:k:_) =
    (SOut (StreamInternal ("output"++show i) (typeOf (undefined :: a)))
      , SOut (StreamInternal ("output"++show j) (typeOf (undefined :: b)))
      , SOut (StreamInternal ("output"++show k) (typeOf (undefined :: c))))


-- Datatype for composition of stream computations 
data Block mem istreams ostreams where
  ComputeBlock :: mem -> istreams -> ostreams -> Compute () -> Block mem istreams ostreams 
  (:>>:) :: Block m i o -> Block m' o o' -> Block (m :+: m') i o'  


-- TODO: Implement mkComputeBlock that hides as much ugliness as possible!!! 
-- mkComputeBlock :: ???? => ? -> ?


-- Identity program test 
test :: Block () (Stream ZInt) (Stream ZInt)
test = ComputeBlock () (S (unsin in1)) (S (unsout out1)) $ f in1 out1 
  where
    f :: Emb a => StreamIn a -> StreamOut a -> Compute () 
    f i o = do {a <- sget i; sput o a}
    -- TODO: Generate these
    (in1,out1) = (streamInputs (undefined :: Stream ZInt) [0..]
                 ,streamOutputs (undefined :: Stream ZInt) [0..]) 
    --in1 = (SIn (StreamInternal "s1" TInt)) :: StreamIn ZInt 
    --out1 = (SOut (StreamInternal "o1" TInt)) :: StreamOut ZInt 

-- Of course it does not work!
-- Since streamIn and StreamOut are different types!
-- Coming up with a way to generate the stream arguments used
-- will also provide a solution to the types issue...
-- A different "Stream-level"-programming type of Streams is needed
-- see above, but also conversions to the lower level streams.

-- Now compTest works! (Tweaks needed!) 
compTest :: Block (() :+: ()) (Stream ZInt) (Stream ZInt)
compTest = test :>>: test 

-- StreamAdd test 
-- test2 :: Block () (StreamIn ZInt, StreamIn ZInt) (StreamOut ZInt)
-- test2 = ComputeBlock () (in1,in2) out1 $ f in1 in2 out1 
--   where
--     f :: (Num a, Emb a) => StreamIn a -> StreamIn a -> StreamOut a -> Compute () 
--     f i1 i2 o = do {a <- sget i1; b <- sget i2; sput o (a + b)}
--     -- TODO: Generate these
--     in1 = (SIn (StreamInternal "s1" TInt)) :: StreamIn ZInt
--     in2 = (SIn (StreamInternal "s2" TInt)) :: StreamIn ZInt 
--     out1 = (SOut (StreamInternal "o1" TInt)) :: StreamOut ZInt 

