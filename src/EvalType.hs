-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import Data.Map
import Data.List
import AST
import Control.Monad.State

insertTypeMap :: Map String [Type] -> String -> Type -> Map String [Type]
-- 加入一个新的绑定（插入到Type数组的头部即可）
insertTypeMap m s t = if s `member` m then update (\ts -> Just (t:ts)) s m else Data.Map.insert s [t] m

findTypeMap :: Map String [Type] -> String -> Maybe Type
-- 查找一个名字对应的类型
findTypeMap m s = if Data.List.null ans then Nothing else Just (ans!!0) where ans = findWithDefault [] s m

deleteTypeMap :: Map String [Type] -> String -> Map String [Type]
-- 删除一个名字的最近的绑定，代码确保名字至少有一个绑定
deleteTypeMap m s = update (\ts -> if length ts == 1 then Nothing else Just (Data.List.drop 1 ts)) s m

type Context = Map String [Type]
type ContextState a = StateT Context Maybe a

isBool :: Expr -> ContextState Type
isBool e = do
  et <- eval e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isInt :: Expr -> ContextState Type
isInt e = do
  et <- eval e
  case et of
    TInt -> return TInt
    _ -> lift Nothing

isChar :: Expr -> ContextState Type
isChar e = do
  et <- eval e
  case et of
    TChar -> return TChar
    _ -> lift Nothing

eval :: Expr -> ContextState Type
eval (EBoolLit _) = return TBool
eval (EIntLit _) = return TInt
eval (ECharLit _) = return TChar
eval (ENot e) = isBool e >> return TBool
eval (EAnd a b) = isBool a >> isBool b >> return TBool
eval (EOr a b) = eval (EAnd a b)
eval (EAdd a b) = isInt a >> isInt b >> return TInt
eval (ESub a b) = eval (EAdd a b)
eval (EMul a b) = eval (EAdd a b)
eval (EDiv a b) = eval (EAdd a b)
eval (EEq a b) =  do
  ta <- eval a
  tb <- eval b
  case ta of
    TBool -> 
      case tb of 
        TBool -> return TBool
        _ -> lift Nothing
    TInt -> 
      case tb of 
        TInt -> return TBool
        _ -> lift Nothing
    TChar -> 
      case tb of 
        TChar -> return TBool
        _ -> lift Nothing
    _ -> lift Nothing
eval (ENeq a b) = eval (EEq a b)
eval (ELt a b) =  do
  ta <- eval a
  tb <- eval b
  case ta of
    TInt -> 
      case tb of 
        TInt -> return TBool
        _ -> lift Nothing
    TChar -> 
      case tb of 
        TChar -> return TBool
        _ -> lift Nothing
    _ -> lift Nothing
eval (EGt a b) = eval (ELt a b)
eval (ELe a b) = eval (ELt a b)
eval (EGe a b) = eval (ELt a b)
eval (EIf a b c) = do
  ta <- eval a
  tb <- eval b
  tc <- eval c
  if ta == TBool && tb == tc then return tb else lift Nothing
eval (ELambda (s, t) e) = do
  context <- get
  put (insertTypeMap context s t)
  te <- eval e
  put context
  return (TArrow t te)
eval (ELet (s, a) b) = do
  ta <- eval a
  context <- get
  put (insertTypeMap context s ta)
  tb <- eval b
  put context
  return tb
eval (EVar s) = do
  context <- get
  case findTypeMap context s of 
    Just x -> return x
    _ -> lift Nothing
eval (EApply a b) = do
  ta <- eval a
  tb <- eval b
  case ta of
    TArrow tt1 tt2 -> if tt1 == tb then return tt2 else lift Nothing
    _ -> lift Nothing
  return tb
eval (ELetRec func (arg, targ) (e, te) exp) = do
  context <- get
  put $ insertTypeMap (insertTypeMap context func (TArrow targ te)) arg targ
  te_eval <- eval e
  if te_eval == te then return te else lift Nothing
  put (insertTypeMap context func (TArrow targ te))
  texp <- eval exp
  put context
  return texp
eval (ECase e pes) = do
  te <- eval e
  if Data.List.null pes then lift Nothing else return TChar
  context <- get
  let types = Data.List.map (\(p, e) -> evalPE context te p e) pes in if (all (\t -> t == (types!!0)) types) then 
    case types!!0 of 
      Just x -> return x
      Nothing -> lift Nothing
  else lift Nothing

evalPE :: Map String [Type] -> Type -> Pattern -> Expr -> Maybe Type
-- 参数1：当前变量绑定情况
-- 参数2：case表达式的类型，需要与Pattern匹配
-- 参数3：匹配的Pattern
-- 参数4：匹配后要计算的表达式
-- 返回：可能的类型
evalPE m t p e = case p of
  PBoolLit _ -> if t == TBool then evalStateT (eval e) m else Nothing
  PIntLit _ -> if t == TInt then evalStateT (eval e) m else Nothing
  PCharLit _ -> if t == TChar then evalStateT (eval e) m else Nothing
  PVar name -> evalStateT (eval e) (insertTypeMap m name t)
  PData name ps -> Nothing -- TODO

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $ empty -- 可以用某种方式定义上下文，用于记录变量绑定状态
