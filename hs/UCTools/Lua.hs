-- Lua tools.  Goals:
-- . Static analysis of code written in Lua (parse, eval)
-- . A tagless final DSL to generate Lua code (compile, pretty)


-- Evaluator is mostly about how to handle local and global variables.
-- I think I'm just going to use a State monad with two maps for this,
-- with the local map being reset during sub-block execution.

-- TODO:
-- . write some code in test_lua_hs.lua and fill in the interpreter

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module UCTools.Lua where

import Language.Lua
import Control.Monad.Writer
import qualified Control.Monad.State as State
import Control.Monad.Fix
import Data.Map
import Control.Lens
import Prelude hiding (log)
import Data.Text

-- class DSL where

type Table = Map Value Value
data Value = VFloat Float
           | VString Text
           | VTable Table
           | VNil
           deriving (Show, Eq, Ord)

data State    = State { _local :: Table, _global :: Table } deriving (Show)
data Result t = Result State Log t deriving (Show)

type Log = String

newtype EvalM t = EvalM (WriterT Log (State.State State) t)
                deriving (Functor, Applicative, Monad,
                          MonadFix,
                          MonadWriter Log,
                          State.MonadState State)

$(makeLenses ''State)

log = tell
logn = log . (++ "\n")

instance MonadFail EvalM where
  fail msg = error $ "EvalM:fail:" ++ msg
  


-- https://hackage.haskell.org/package/language-lua-0.11.0.2/docs/Language-Lua-Syntax.html

evalBlock block@(Block stats maybeReturn) = do
  logn $ show block
  saveLocal <- use local
  traverse evalStat stats
  local .= saveLocal
  case maybeReturn of
    Nothing -> return [VNil]
    Just exprs  -> do
      -- FIXME: Semantics not clear, how do multiple return values combine?
      vals <- traverse evalExp exprs
      return $ Prelude.concat vals
  
evalStat stat = do
  -- log $ show stat
  let ni str = logn $ "Stat not implemented: " ++ str
      ev (Assign vars exprs) = ni "Assign"
      ev (FunCall funCall) = ni "FunCall"
      ev (Label name) = ni "Label"
      ev Break = ni "Break"
      ev (Goto name) = ni "Goto"
      ev (Do block) = ni "Do"
      ev (While exp block) = ni "While"
      ev (Repeat block exp) = ni "Repeat"
      ev (If expBlocks maybeElse) = ni "If"
      ev (ForRange name expr exp maybeExp block) = ni "ForRange"
      ev (ForIn names exprs block) = ni "ForIn"
      ev ex@(FunAssign (FunName name names maybeName) body) = do
        -- name: Scoped variable (global or local)
        -- names: Dotted keys into table
        -- maybeName: last name in path is a method name with implicit self arg
        -- Translate that to variable + map path + boolean for method
        logn $ show ex
        ni "FunAssign"
      ev (LocalFunAssign name body) = ni "LocalFunAssign"
      ev ex@(LocalAssign (Name v:[]) (Just [expr])) = do
        -- FIXME: Zip semantics is not clear
        -- logn $ show ex
        [val] <- evalExp expr
        local %= insert (VString v) val
      ev ex@(LocalAssign _ _) = do
        ni $ "LocalAssign: " ++ show ex
      ev EmptyStat = ni "EmptyStat"
  ev stat

-- Varaibles can only have one value.  Function calls can have
-- multiple.  This is awkward.  It seems simples to always use [Value]
-- instead of Value as eval return value.
evalVar :: Var -> EvalM [Value]
evalVar var = do
  logn $ "Var not implemented: " ++ show var ++ "\n"
  return [VNil]

evalFunCall fc = do
  logn $ "FunCall not implemented: " ++ show fc ++ "\n"
  return [VNil]

evalExp :: Exp -> EvalM [Value]  
evalExp expr = do
  -- log $ show expr
  let ni str = do logn $ "Expr not implemented: " ++ str ; return [VNil]
      ev Nil = ni "Nil"
      ev (Bool bool) = ni "Bool"
      ev (Number typ text) = ni "Number"
      ev (String text) = ni "String"
      ev Vararg = ni "Vararg"
      ev (EFunDef body) = ni "EFunDef"
      ev (PrefixExp pexp) = case pexp of
        PEVar v      -> evalVar v
        Paren e      -> evalExp e
        PEFunCall fc -> evalFunCall fc
      ev (TableConst tableFields) = ni "TableConst"
      ev (Binop binop e1 e2) = ni "Binop"
      ev (Unop unop e) = ni "Unop"
  ev expr

-- The result of parseFile is a Block.  It seems best to focus on the
-- file level.

eval :: Block -> Result [Value]
eval block = Result s l v where
  EvalM m = evalBlock block
  s0 = State mempty mempty
  ((v, l), s) = State.runState (runWriterT m) s0

test1 = do
  rv <- parseFile "../lua/test_lua_hs.lua"
  case rv of
    Right block -> do
      -- putStrLn $ show block
      let doc = pprint block
      -- putStrLn $ show doc
      let (Result state log rv) = eval block
      -- putStrLn $ show state
      putStrLn $ log
      putStrLn $ show rv
    Left (sourceRange, msg) -> do
      putStrLn $ show sourceRange
      putStrLn $ msg
      
