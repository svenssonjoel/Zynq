

module Zincite.Backend.BaseType where

import Zincite.Syntax

import Data.Word

-- Reify base types
class BaseType a where
  baseType :: a -> Type


-- Repeat many times ;) 
instance BaseType Int where
  baseType _ = TInt

instance BaseType Word where
  baseType _ = TUInt

instance BaseType Float where
  baseType _ = TFloat

instance BaseType Bool where
  baseType _ = TBool


