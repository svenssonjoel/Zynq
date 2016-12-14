{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- ***** {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}


module Zincite.Syntax where 

import Control.Monad.State  hiding (forever)
import Control.Monad.Writer hiding (forever)
import Control.Applicative

import GHC.TypeLits

 -- import Data.Type.List  -- Currently dont need it 
import Data.Word 

type Name = String 
type Address = Word
type Size = Int

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
  | Declare Name Type                 -- Variable declaration
  | MWrite MemoryInternal  e e Type   -- DRAM Write 
  | LocalMemory MemoryInternal        -- Request use of Local Ram  
  | SGet   Target StreamInternal Type -- Pop of a Stream 
  | SPut   StreamInternal e Type      -- Push onto a Stream 
  | Assign Target  e Type             -- Assignment to variable 
  | Seq    (Code e) (Code e)          -- sequencing of operation
  | While  Name (Code e) e            -- Name bodyCode cond
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

-- TODO: Maybe should be class of its own. 
--       This way we can have a special implementation of mread
--       for tuple values (more than one read). 
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

-- Why is this not already available ??
-- Concatenation of type level lists 
type family (++) (a :: [k])  (b :: [k]) :: [k] where
  (++) a  '[] = a
  (++) '[] a  = a 
  (++) (a ': as)  bs = a ': (as ++ bs) 

-- The Block language differentiates between input and output streams
-- byt their position in the argument list. 
data Stream a = S StreamInternal 

-- Datatype for composition of stream computations 
data Block (mem :: [k]) (istreams :: [k]) (ostreams :: [k])  where

  ComputeBlock :: Interfaces mem istreams ostreams
               -> Compute ()
               -> Block mem istreams ostreams               
       -- mem, istreams, ostreams are just there to lock down the types
       -- (sorry for inexact vocabulary)
       -- This is because the preferred way to create these "Compute ()"
       -- objects is by normal haskell functions a -> .. -> Compute ()
       -- where the arguments (a ..) are memory interfaces, streams, etc.
       -- The type information present in the inputs to the function
       -- are forgotten when applied (at haskell type level), but we
       -- still want to know this when composing Blocks. 
  
  (:>>:) :: Block m i o -> Block m' o o' -> Block (m ++ m') i o'

  -- Woa, writing the connectors is tricky! 
  ConnectHead :: Block m i (o ': os)
              -> Block m' '[o] o'
              -> Block (m ++ m') i (o' ++ os) 

  ConnectTail :: Block m i (o ': os) 
              -> Block m' os o' 
              -> Block (m ++ m') i ('[o] ++ o') 
  
  
  -- TODO: Come up with more ways to compose Blocks 


-- TODO: Implement mkComputeBlock that hides as much ugliness as possible!!!

data Interfaces (a :: [k]) (b :: [k]) (c :: [k])  where
  Interfaces :: [MemoryInternal] -- Memory IO  
             -> [StreamInternal] -- Input Streams
             -> [StreamInternal] -- Output Streams 
             -> [(Name,Type)]    -- Control arguments 
             -> Interfaces a b c 

emptyInterfaces :: Interfaces '[] '[] '[] 
emptyInterfaces = Interfaces [] [] [] [] 

addMem :: Interfaces m i o -> (Name, m') -> Interfaces (m' ': m) i o  
addMem (Interfaces m i o c) (nom,_) = Interfaces (InterfaceIO nom :m) i o c

addIStream :: Interfaces m i o -> (Name ,i') -> Type -> Interfaces m (i' ': i) o 
addIStream (Interfaces m i o c) (nom,_) t = Interfaces m (StreamInternal nom t : i) o c

addOStream :: Interfaces m i o -> (Name, o') -> Type -> Interfaces m i (o' ': o) 
addOStream (Interfaces m i o c) (nom,_) t = Interfaces m i (StreamInternal nom t : o) c

addCArg :: Interfaces m i o -> Name -> Type -> Interfaces m i o
addCArg (Interfaces m i o c) nom t = Interfaces m i o ((nom,t):c)


-- Create an Interfaces object from the type of a function that
-- returns a Compute ().
type GenIF a = State (Int,Int,Int,Int) a
runGenIF :: GenIF a -> a 
runGenIF g = evalState g (0,0,0,0) 

genNewMem, genNewIStream, genNewOStream, genNewCArg :: GenIF String
genNewMem = do {(i,j,k,l) <- get; put (i+1,j,k,l); return ("mem" ++ show i)}
genNewIStream = do {(i,j,k,l) <- get; put (i,j+1,k,l); return ("istream" ++ show j)}
genNewOStream = do {(i,j,k,l) <- get; put (i,j,k+1,l); return ("ostream" ++ show k)}
genNewCArg = do {(i,j,k,l) <- get; put (i,j,k,l+1); return ("c" ++ show l)}


class GenInterfaces a mem i o where
  genInterfaces :: a -> GenIF (Interfaces mem i o, Compute ())

-- Base case for recursion
instance GenInterfaces (Compute ()) '[] '[] '[]  where
  genInterfaces c = return (emptyInterfaces,c)

instance GenInterfaces b m i o => GenInterfaces (Memory Global -> b) (Memory Global ': m) i o where
  genInterfaces f =
    do
      mem_nom <- genNewMem
      let mem = M (InterfaceIO mem_nom)
          rest = f mem
      (ifs,c) <- genInterfaces rest
      return (addMem ifs (mem_nom, undefined :: Memory Global), c) 

instance (Emb a, GenInterfaces b m i o) => GenInterfaces (StreamIn a -> b) m (Stream a ': i) o where
  genInterfaces f =
    do
      i_nom <- genNewIStream
      let i_var = SIn (StreamInternal i_nom t)
          rest = f i_var
      (ifs,c) <- genInterfaces rest
      return (addIStream ifs (i_nom, undefined :: Stream a) t,c)
      where
        t = typeOf (undefined :: a) 

instance (Emb a, GenInterfaces b m i o) => GenInterfaces (StreamOut a -> b) m i (Stream a ': o) where
  genInterfaces f =
    do
      i_nom <- genNewOStream
      let i_var = SOut (StreamInternal i_nom t)
          rest = f i_var
      (ifs,c) <- genInterfaces rest
      return (addOStream ifs (i_nom, undefined :: Stream a)  t,c)
      where
        t = typeOf (undefined :: a)

instance (Emb a, GenInterfaces b m i o) => GenInterfaces (a -> b) m i o where
  genInterfaces f =
    do
      i_nom <- genNewCArg
      let i_var = fromExp (variableE i_nom t)
          rest = f i_var
      (ifs,c) <- genInterfaces rest
      return (addCArg ifs i_nom t,c)
      where
        t = typeOf (undefined :: a)
                 
-- mkComputeBlock :: ???? => ? -> ?

-- Identity program test 
test :: Block '[] ('[Stream ZInt] ) ('[Stream ZInt])
test = ComputeBlock ifs c 
  where
    (ifs,c) = runGenIF (genInterfaces f)
    
    f :: StreamIn ZInt -> StreamOut ZInt -> Compute () 
    f i o = do {a <- sget i; sput o a}


-- NOW testWrong leads to an error "No instance for GenInterfaces..." 
-- testWrong :: Block () (Stream ZInt) (Stream ZInt)
-- testWrong = ComputeBlock ifs c 
--   where
--     (ifs,c) = runGenIF (genInterfaces f)
--     -- This is clearly wrong! (How to protect against it ?)
--     -- Need a relation between the type of testWrong and f. 
--     -- and genInterfaces needs to know about this as well.. 
--     f ::  StreamIn ZFloat -> StreamOut ZFloat -> Compute () 
--     f i o = do {a <- sget i; sput o a}

--compTest :: Block (() :+: ()) (Stream ZInt :+: ()) (Stream ZInt :+: () )
compTest :: Block '[] '[Stream ZInt] '[Stream ZInt]
compTest = test :>>: test :>>: test 

-- streamAdd test 
test2 :: Block '[] '[Stream ZInt, Stream ZInt] '[Stream ZInt]
test2 = ComputeBlock ifs c 
  where
    (ifs,c) = runGenIF (genInterfaces f)
    f :: StreamIn ZInt -> StreamIn ZInt -> StreamOut ZInt -> Compute () 
    f i1 i2 o = do {a <- sget i1; b <- sget i2; sput o (a + b)}
   
