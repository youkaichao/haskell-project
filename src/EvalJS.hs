-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalJS :: Program -> String 就行。
module EvalJS where

import Text.Printf
import Data.List
import AST
import Control.Monad.State
import qualified EvalType

eval :: Expr -> String
eval (EBoolLit b) = if b then "true" else "false"
eval (EIntLit b) = show b
eval (ECharLit b) = "'" ++ (b:[]) ++"'"
eval (ENot e) = printf "! %s" $ eval e
eval (EAnd a b) = printf "!!(%s) && !!(%s)" (eval a) (eval b)
eval (EOr a b) = printf "!!(%s) || !!(%s)" (eval a) (eval b)
eval (EAdd a b) = printf "(%s) + (%s)" (eval a) (eval b)
eval (ESub a b) = printf "(%s) - (%s)" (eval a) (eval b)
eval (EMul a b) = printf "(%s) * (%s)" (eval a) (eval b)
eval (EDiv a b) = printf "Math.floor((%s) / (%s))" (eval a) (eval b)
eval (EMod a b) = printf "(%s) %% (%s)" (eval a) (eval b)
eval (EEq a b) =  printf "(%s) === (%s)" (eval a) (eval b)
eval (ENeq a b) = printf "(%s) !== (%s)" (eval a) (eval b)
eval (ELt a b) =  printf "(%s) < (%s)" (eval a) (eval b)
eval (EGt a b) =  printf "(%s) > (%s)" (eval a) (eval b)
eval (ELe a b) =  printf "(%s) <= (%s)" (eval a) (eval b)
eval (EGe a b) =  printf "(%s) >= (%s)" (eval a) (eval b)
eval (EIf a b c) = printf "(%s)?(%s):(%s)" (eval a) (eval b) (eval c)
eval (ELambda (s, t) e) = printf "function (%s) {return (%s)}" s (eval e)
eval (ELet (s, a) b) = printf "(function (%s) {return (%s)})(%s)" s (eval b) (eval a)
eval (ELetRec func (arg, targ) (e, te) exp) = printf "(function (%s){return %s})(function %s(%s){return %s})" func (eval exp) func arg (eval e)
eval (EVar s) = s
eval (EApply a b) = printf "(%s)(%s)" (eval a) (eval b)
eval (ECase e pes) = let tmp_name = "$_tmp" in printf "{%s=(%s);%s}" tmp_name (eval e) (pattern tmp_name pes)

pattern :: [Char] -> [(Pattern, Expr)] -> [Char]
pattern tmp_name [] = "undefined"
pattern tmp_name ((p, e):xs) = case p of
  PBoolLit b -> printf "(%s===%s)?(%s):(%s)" tmp_name (if b then "true" else "false") (eval e) (pattern tmp_name xs)
  PIntLit b -> printf "(%s===%s)?(%s):(%s)" tmp_name (show b) (eval e) (pattern tmp_name xs)
  PCharLit b -> printf "(%s===%s)?(%s):(%s)" tmp_name (b:[]) (eval e) (pattern tmp_name xs)
  PVar b -> printf "(%s===%s)?(function (%s){return %s})(%s):(%s)" tmp_name tmp_name b (eval e) tmp_name (pattern tmp_name xs)
    

evalJS :: Program -> String
evalJS (Program adts body) = eval body