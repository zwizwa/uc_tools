-- NEXT:
-- . write more code in test_lua_hs.lua and fill in the interpreter
-- . represent functions as code or syntax?

-- TODO: Tables really need to be accessed by reference.  It is not
-- enough to just add an id.


-- Lua tools.  Goals:
-- . Static analysis of code written in Lua (parse, eval)
-- . A tagless final DSL to generate Lua code (compile, pretty)


-- Evaluator is mostly about how to handle local and global variables.
-- I think I'm just going to use a State monad with two maps for this,
-- with the local map being reset during sub-block execution.



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
import Data.IntMap
import Data.List
import Control.Lens
import Prelude hiding (log)
import Data.Text
import Data.Text.Read
import Text.Pretty.Simple

type Table = Map Value Value
data Value = VFloat Float
           | VInt Int
           | VString Text
           | VNil
           | VNI String
           | VPtr Int
           deriving (Show, Eq, Ord)

-- Tables and functions are compared by reference, and tables can be
-- updated in-place.  I don't see a way to avoid putting them in a
-- store.  I also don't see how I can avoid then manually implementing
-- GC on that store.
data Object = OTable Table
            | OFun   (Maybe Int) FunBody
            deriving (Show, Eq)

-- I think that 

data State    = State {
  _nextId  :: Int,
  _objects :: IntMap Object
  } deriving (Show)
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
  


-- Update nested map from list of keys.  The generic method can use
-- any Value to index.

-- insertPath :: Value -> [ Value ] -> Table -> Table
-- insertPath v = u where
--   u [k] = Data.Map.insert k v
--   u (k:ks) = Data.Map.adjust f k where
--     f (VPtr (OTable id t)) = VPtr $ OTable id $ u ks t

-- FIXME: I just want to make a lens for it.

-- Tables are stored in the objects IntMap and use indirection.
setTableField :: Int -> Value -> Value -> EvalM ()
setTableField id k v = do
  -- objects %= Data.IntMap.adjust id (Data.Map.insert key val)
  -- FIXME: This unrap/rewrap can be done with lenses as well
  -- The fmap is for Maybe
  -- objects . at id %= fmap (\(OTable t) -> OTable $ Data.Map.insert k v t)
  -- I do need a match error when the Table at id doesn't exist.
  objects . at id %= \(Just (OTable t)) -> Just $ OTable $ Data.Map.insert k v t

getTable :: Int -> EvalM Table
getTable id = do
  Just (OTable t) <- use (objects . at id)
  return t

getTableField :: Int -> Value -> EvalM Value
getTableField id k = do
  t <- getTable id
  case Data.Map.lookup k t of
    Just v  -> return v
    Nothing -> return VNil

getLocals  = getTable locals
getGlobals = getTable globals

setTable :: Int -> Table -> EvalM ()
setTable id tab = do
  objects . at id .= (Just $ OTable tab)

setLocals  = setTable locals
setGlobals = setTable globals

setLocal  = setTableField locals   . VString
setGlobal = setTableField globals  . VString

getLocal  = getTableField locals  . VString
getGlobal = getTableField globals . VString

setVar :: Text -> Value -> EvalM ()
setVar name val = do
  l <- getLocal name
  case l of
    VNil -> setGlobal name val
    _    -> setLocal  name val

getVar :: Text -> EvalM Value
getVar name = do
  l <- getLocal name
  case l of
    VNil -> getGlobal name
    _    -> return l
      


evalBlock block@(Block stats maybeReturn) = do
  -- logn $ show block
  locals' <- getLocals
  traverse evalStat stats
  rvs <- case maybeReturn of
    Nothing -> return [VNil]
    Just exprs  -> do
      -- FIXME: Semantics not clear, how do multiple return values combine?
      vals <- traverse evalExp exprs
      return $ Prelude.concat vals
  setLocals locals'
  return rvs

unName (Name n) = n
unNames = fmap unName
namesToPath = fmap (VString . unName)

traverse' = flip traverse

makeTable = do
  id <- use nextId
  nextId %= (+ 1)
  objects %= Data.IntMap.insert id (OTable mempty)
  return id

makeFun :: (Maybe Int) -> FunBody -> EvalM Int
makeFun maybeSelf body = do
  id <- use nextId
  nextId %= (+ 1)
  objects %= Data.IntMap.insert id (OFun maybeSelf body)
  return id

    

setVarPath :: Value -> [Name] -> EvalM ()
setVarPath val = s where
  s _ = do
    logn $ "setVarPath"
    return ()
  
evalStat stat = do
  -- log $ show stat
  let ni str = logn $ "Stat not implemented: " ++ str
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
      ev EmptyStat = ni "EmptyStat"

      ev ex@(FunAssign (FunName name names maybeName) body) = do
        let ns = [name] ++ names
        (allNames, maybeSelf) <-
          case maybeName of
            Nothing -> do
              return $ (ns ++ [],  Nothing)
            Just n  -> do
              return $ (ns ++ [n], Just (-1))  -- FIXME: Get table ref
        -- logn $ "body: " ++ show body
        id <- makeFun maybeSelf body
        setVarPath (VPtr id) allNames
          
      ev ex@(LocalFunAssign (Name name) body) = do
        logn $ "LocalFunAssign: " ++ show ex
        id <- makeFun Nothing body
        setLocal name $ VPtr id
        
      ev ex@(LocalAssign names maybeExps) = do
        -- logn $ "LocalAssign: " ++ show ex
        vals' <- case maybeExps of
          Just exps ->
            traverse evalExp exps
          Nothing ->
            -- FIXME: Not clear.  All nil?
            return $ cycle [[VNil]]
        let vals = Prelude.concat vals'
            -- FIXME: Zip vars with values correct?
            nvs = Prelude.zip names vals 
        traverse' nvs $ \(Name name, val) -> do
          setTableField locals (VString name) val
        return ()

      ev (Assign vars exps) = do
        vals <- traverse evalExp exps
        let assign (k, v) = do
              logn $ "assign " ++ show k ++ " " ++ show v
              return ()
        traverse assign $ Prelude.zip vars $ Prelude.concat vals
        return ()
      
  ev stat

-- Varaibles can only have one value.  Function calls can have
-- multiple.  This is awkward.  It seems simples to always use [Value]
-- instead of Value as eval return value.
evalVar :: Var -> EvalM [Value]
evalVar var@(VarName (Name name)) = do
  val <- getVar name
  return [val]

evalFunCall fc = do
  logn $ "FunCall not implemented: " ++ show fc ++ "\n"
  return [VNI "evalFunCall"]

evalExp :: Exp -> EvalM [Value]  
evalExp expr = do
  -- log $ show expr
  let ni str = do logn $ "Expr not implemented: " ++ str ; return [VNI str]
      ev Nil = ni "Nil"
      ev (Bool bool) = ni "Bool"
      ev (String text) = ni "String"
      ev Vararg = ni "Vararg"
      ev (EFunDef body) = ni "EFunDef"
      ev (Binop binop e1 e2) = ni "Binop"
      ev (Unop unop e) = ni "Unop"

      ev ex@(Number IntNum text) = do
        -- FIXME use Data.Text.Read
        let Right (n, _) = decimal text
        return [ VInt n ]

      ev (PrefixExp pexp) = case pexp of
        PEVar v      -> evalVar v
        Paren e      -> evalExp e
        PEFunCall fc -> evalFunCall fc

      ev (TableConst tableFields) = do
        -- [1..] as ExpField.  Then traverse evalExp and store as map.
        let -- Array fields and keyed fields are handled separately
            isField (Field _) = True ; isField _ = False
            (arrFields',keyFields) = Data.List.partition isField tableFields
            arrFields = Prelude.zip [1..] arrFields'

            -- Array values
            indexed :: (Int, TableField) -> EvalM (Value, Value)
            indexed (i, Field v) = do
              [v'] <- evalExp v
              return (VInt i, v')

            keyed :: TableField -> EvalM (Value, Value)
            keyed (NamedField (Name k) v) = do
              [v'] <- evalExp v
              return (VString k, v')
            keyed (ExpField k v) = do
              [k'] <- evalExp k
              [v'] <- evalExp v
              return (k', v')

        assIndexed <- traverse indexed arrFields
        assKeyed   <- traverse keyed   keyFields
        id <- makeTable
        setTable id $ Data.Map.fromList $ assIndexed ++ assKeyed  
        
        return $ [ VPtr id ]
        
  ev expr

-- The result of parseFile is a Block.  It seems best to focus on the
-- file level.

-- These can just be hardcoded
globals = 0
locals  = 1


runM :: EvalM t -> Result t
runM (EvalM m) = Result s l v where
  to0 = OTable mempty
  obj0 = Data.IntMap.fromList [(globals, to0), (locals, to0)]
  s0 = State 2 obj0
  ((v, l), s) = State.runState (runWriterT m) s0

eval = runM . evalBlock

-- eval :: Block -> Result [Value]
-- eval block = Result s l v where
--   EvalM m = evalBlock block
--   s0 = State mempty mempty 0
--   ((v, l), s) = State.runState (runWriterT m) s0

pp = pPrintNoColor

test1 = do
  putStrLn "** test1"
  rv <- parseFile "../lua/test_lua_hs.lua"
  case rv of
    Right block -> do
      let (Result state log rv) = eval block
      putStr $ log
      pp state
      pp rv
    Left (sourceRange, msg) -> do
      putStrLn $ show sourceRange
      putStrLn $ msg

test2 = do
--   putStrLn "** test2"
--   let
--     m = do
--       id0 <- makeId
--       id1 <- makeId
           
--       let t0 = insertPath VNil                   [VString "a"] mempty
--           t1 = insertPath (VPtr (OTable id0 t0)) [VString "c"] mempty
--           t2 = insertPath VNil                   [VString "c", VString "b"] t1
--       return t2

--     (Result state log rv) = runM m
--   pp rv
  return ()
