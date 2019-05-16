-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import Data.Map
import Data.List
import AST
import Control.Monad.State
import qualified EvalType

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VFunc String Expr -- 函数类型，记录第一个参数的参数名和函数体
  -- ... more
  deriving (Show, Eq)

insertValueMap :: Map String [Value] -> String -> Value -> Map String [Value]
-- 加入一个新的绑定（插入到Value数组的头部即可）
insertValueMap m s t = if s `member` m then update (\ts -> Just (t:ts)) s m else Data.Map.insert s [t] m

findValueMap :: Map String [Value] -> String -> Maybe Value
-- 查找一个名字对应的类型
findValueMap m s = if Data.List.null ans then Nothing else Just (ans!!0) where ans = findWithDefault [] s m

deleteValueMap :: Map String [Value] -> String -> Map String [Value]
-- 删除一个名字的最近的绑定，代码确保名字至少有一个绑定
deleteValueMap m s = update (\ts -> if length ts == 1 then Nothing else Just (Data.List.drop 1 ts)) s m

type Context = Map String [Value]

type ContextState a = StateT Context Maybe a

getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

eval :: Expr -> ContextState Value
eval (EBoolLit b) = return $ VBool b
eval (EIntLit b) = return $ VInt b
eval (ECharLit b) = return $ VChar b
eval (ENot e) = do
  ev <- eval e
  case ev of 
    VBool b -> return (VBool $ not b)
    _ -> lift Nothing
eval (EAnd a b) = do
  va <- eval a
  case va of 
    VBool realVa -> if realVa == False then return (VBool False) else
      do
        vb <- eval b
        case vb of
          VBool realVb -> return (VBool realVb)
          _ -> lift Nothing
    _ -> lift Nothing
eval (EOr a b) = do
  va <- eval a
  case va of 
    VBool realVa -> if realVa == True then return (VBool True) else
      do
        vb <- eval b
        case vb of
          VBool realVb -> return (VBool realVb)
          _ -> lift Nothing
    _ -> lift Nothing
eval (EAdd a b) = do
  va <- eval a
  vb <- eval b
  case va of 
    VInt realVa -> case vb of
                      VInt realVb -> return (VInt (realVa + realVb))
                      _ -> lift Nothing
    _ -> lift Nothing
eval (ESub a b) = do
  va <- eval a
  vb <- eval b
  case va of 
    VInt realVa -> case vb of
                      VInt realVb -> return (VInt (realVa - realVb))
                      _ -> lift Nothing
    _ -> lift Nothing
eval (EMul a b) = do
  va <- eval a
  vb <- eval b
  case va of 
    VInt realVa -> case vb of
                      VInt realVb -> return (VInt (realVa * realVb))
                      _ -> lift Nothing
    _ -> lift Nothing
eval (EDiv a b) = do
  va <- eval a
  vb <- eval b
  case va of 
    VInt realVa -> case vb of
                      VInt realVb -> if realVb == 0 then lift Nothing else return (VInt (div realVa realVb))
                      _ -> lift Nothing
    _ -> lift Nothing
eval (EEq a b) =  do
  va <- eval a
  vb <- eval b
  case va of
    VBool realVa -> 
      case vb of 
        VBool realVb -> return $ VBool $ realVa == realVb
        _ -> lift Nothing
    VInt realVa -> 
      case vb of 
        VInt realVb -> return $ VBool $ realVa == realVb
        _ -> lift Nothing
    VChar realVa -> 
      case vb of 
        VChar realVb -> return $ VBool $ realVa == realVb
        _ -> lift Nothing
eval (ENeq a b) = do
  ans <- eval (EEq a b)
  case ans of
    VBool realV -> return $ VBool $ not realV
    _ -> lift Nothing
eval (ELt a b) =  do
  va <- eval a
  vb <- eval b
  case va of
    VInt realVa -> 
      case vb of 
        VInt realVb -> return $ VBool $ realVa < realVb
        _ -> lift Nothing
    VChar realVa -> 
      case vb of 
        VChar realVb -> return $ VBool $ realVa < realVb
        _ -> lift Nothing
    _ -> lift Nothing
eval (EGt a b) =  do
  va <- eval a
  vb <- eval b
  case va of
    VInt realVa -> 
      case vb of 
        VInt realVb -> return $ VBool $ realVa > realVb
        _ -> lift Nothing
    VChar realVa -> 
      case vb of 
        VChar realVb -> return $ VBool $ realVa > realVb
        _ -> lift Nothing
    _ -> lift Nothing
eval (ELe a b) =  do
  va <- eval a
  vb <- eval b
  case va of
    VInt realVa -> 
      case vb of 
        VInt realVb -> return $ VBool $ realVa <= realVb
        _ -> lift Nothing
    VChar realVa -> 
      case vb of 
        VChar realVb -> return $ VBool $ realVa <= realVb
        _ -> lift Nothing
    _ -> lift Nothing
eval (EGe a b) =  do
  va <- eval a
  vb <- eval b
  case va of
    VInt realVa -> 
      case vb of 
        VInt realVb -> return $ VBool $ realVa >= realVb
        _ -> lift Nothing
    VChar realVa -> 
      case vb of 
        VChar realVb -> return $ VBool $ realVa >= realVb
        _ -> lift Nothing
    _ -> lift Nothing
eval (EIf a b c) = do
  va <- eval a
  case va of
    VBool realVa -> let target = if realVa then b else c in do
      vtarget <- eval target
      return vtarget
    _ -> lift Nothing
eval (ELambda (s, t) e) = do
  return $ VFunc s e
eval (ELet (s, a) b) = do
  va <- eval a
  context <- get
  put (insertValueMap context s va)
  vb <- eval b
  put context
  return vb
eval (ELetRec func (arg, targ) (e, te) exp) = do
  context <- get
  put (insertValueMap context func $ VFunc arg e)
  vexp <- eval exp
  put context
  return vexp
eval (EVar s) = do
  context <- get
  case findValueMap context s of 
    Just x -> return x
    _ -> lift Nothing
eval (EApply a b) = do
  vb <- eval b
  va <- eval a
  case va of
    VFunc s e -> do
      context <- get
      put (insertValueMap context s vb)
      ve <- eval e
      put context
      return ve
    _ -> lift Nothing
eval (ECase e pes) = do
  ve <- eval e
  context <- get
  let (done, v) = Data.List.foldl (\(done, v) (p, e) -> evalPE context ve (done, v) p e) (False, Nothing) pes in 
    if not done then lift Nothing else 
      case v of 
        Just realV -> return realV
        Nothing -> lift Nothing

evalPE :: Map String [Value] -> Value -> (Bool, Maybe Value) -> Pattern -> Expr -> (Bool, Maybe Value)
-- 参数1：当前变量绑定情况
-- 参数2：要匹配的值
-- 参数3：当前的value (元组第一个元素表示求值是否结束。求值结束时，值在第二个元素中（Nothing表示求值出错）；没结束时，第二个元素无意义。)
evalPE m srcValue (done, v) p e
  | done = (True, v)
  | otherwise = case p of
    PBoolLit destBool -> case srcValue of
      VBool realV -> if realV == destBool then (True, evalStateT (eval e) m) else (False, Nothing)
      _ -> (False, Nothing)
    PIntLit destInt -> case srcValue of
      VInt realV -> if realV == destInt then (True, evalStateT (eval e) m) else (False, Nothing)
      _ -> (False, Nothing)
    PCharLit destChar -> case srcValue of
      VChar realV -> if realV == destChar then (True, evalStateT (eval e) m) else (False, Nothing)
      _ -> (False, Nothing)
    PVar destVar -> (True, evalStateT (eval e) (insertValueMap m destVar srcValue))
    _ -> (True, Nothing) -- TODO 代数数据类型，不支持

evalExprValue e = runStateT (eval e) empty

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) empty


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
