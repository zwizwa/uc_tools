-- See README.md

-- The point of this file is to create models for embedded code.  It
-- is inspired by recent discovery of data-reify module which allows
-- extraction of graphs from Haskell data structures, simplifying the
-- use of Haskell as a DSL embedding language by avoiding monads in
-- tagless final DSLs (if they are only there to capture sharing).

{-# LANGUAGE OverloadedStrings #-}

import qualified UCTools.Lua as Lua

main = do
  putStrLn "uc-tools.hs"
  Lua.test1
              
  

  
