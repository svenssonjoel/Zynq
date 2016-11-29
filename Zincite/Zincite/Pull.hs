

-- Pull abstraction 
module Zincite.Pull (Pull, pull, pullMemory) where

import Zincite.Syntax


--TODO: Int or Word for indices ?
--TODO: addr + ix * sizeof(T) or addr + ix ??



data Pull a = Pull (Expr Int) (Expr Address -> a) 


pull :: Expr Int -> (Expr Address -> a) -> Pull a
pull = Pull


pullMemory :: MemoryIO a => Memory m -> Expr Address -> Expr Int -> Pull (Expr a)
pullMemory io addr num = 
  Pull num (\ix -> mread io (addr + ix)) -- Figure out what to do about the pointer arithmetic 



------------------------------------------------------------
-- TODO: Pull arrays here have dynamic size.
--       Which means we need to create loops over them in the target language
--       if we want to for example fold.

