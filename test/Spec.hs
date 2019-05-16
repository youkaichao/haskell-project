import AST
import EvalValue
import EvalType
import Control.Monad.State
import Test.QuickCheck

makeFun :: (String, Type) -> [(String, Type)] -> Expr -> (Expr -> Expr)
makeFun (fn, rt) ((p, t):pts) body =
  let helper [] = body
      helper ((p0, t0):rs) = ELambda (p0, t0) (helper rs)
      ts = map snd pts ++ [rt]
  in ELetRec fn (p, t) (helper pts, foldr1 TArrow ts)

callFun :: Expr -> [Expr] -> Expr
callFun f [e] = EApply f e
callFun f (e:es) = callFun (EApply f e) es

test_sum3 =
  Program [] $
  makeFun ("sum3", TInt) [("x", TInt), ("y", TInt), ("z", TInt)] (EAdd (EAdd (EVar "x")(EVar "y")) (EVar "z")) $ callFun (EVar "sum3") [EIntLit 1, EIntLit 2, EIntLit 3]

test_fbi =
  makeFun ("fbi", TInt) [("x", TInt)] (
  EIf (ELe (EVar "x") (EIntLit 1))
    (EIntLit 1)
    (EAdd
      (EApply (EVar "fbi") (ESub (EVar "x") (EIntLit 1)))
      (EApply (EVar "fbi") (ESub (EVar "x") (EIntLit 2))))
  ) $ callFun (EVar "fbi") [EIntLit 10]

test_stuck =
  makeFun ("stuck", TBool) [("x", TBool)] (
    ENot (EApply (EVar "stuck") (EVar "x")))
  $ callFun (EVar "stuck") [EBoolLit True]

test_adt_ctor =
  Program
  [ ADT "tuple3" [("tuple3", [TInt, TInt, TInt])]
  ] $

  makeFun ("sum3", TInt) [("x", TInt), ("y", TInt), ("z", TInt)] (
  EAdd (EAdd (EVar "x") (EVar "y")) (EVar "z")
  ) $
  ELet ("v", callFun (EVar "tuple3") [EIntLit 1, EIntLit 2, EIntLit 3]) $
  ECase (EVar "v")
  [ (PData "tuple3" [PVar "x", PVar "y", PVar "z"],
     callFun (EVar "sum3") [EVar "x", EVar "y", EVar "z"])
  , (PVar "_",
     EIntLit 2333333)
  ]

test_adt_case =
  Program
  [ ADT "tuple3" [("tuple3", [TInt, TInt, TInt])]
  ] $

  ELet ("x", callFun (EVar "tuple3") [EIntLit 1, EIntLit 2, EIntLit 3]) $
  ECase (EVar "x")
  [ (PData "tuple3" [PIntLit 1, PIntLit 3, PVar "y"],
     ECharLit '1')
  , (PData "tuple3" [PIntLit 1, PIntLit 2, PVar "y"],
     ECharLit '2')
  ]

test_adt_list =
  Program
  [ ADT "List" [ ("Cons", [TInt, TData "List"])
               , ("Nil", [])
               ]
  ] $

  ELet ("x", EVar "Nil") $
  ELet ("y", callFun (EVar "Cons") [EIntLit 2, EVar "x"]) $
  ELet ("z", callFun (EVar "Cons") [EIntLit 3, EVar "y"]) $
  EVar "z"

test_adt_list_range =
  Program
  [ ADT "List" [ ("Cons", [TInt, TData "List"])
               , ("Nil", [])
               ]
  ] $

  makeFun ("range", TData "List") [("n", TInt)] (
  EIf (ELe (EVar "n") (EIntLit 0))
    (EVar "Nil")
    (callFun (EVar "Cons") [EVar "n", callFun (EVar "range") [ESub (EVar "n") (EIntLit 1)]])
  ) $
  
  callFun (EVar "range") [EIntLit 10]

test_adt_list_sum =
  Program
  [ ADT "List" [ ("Cons", [TInt, TData "List"])
               , ("Nil", [])
               ]
  ] $

  makeFun ("range", TData "List") [("n", TInt)]
  (
    EIf (ELe (EVar "n") (EIntLit 0))
    (EVar "Nil")
    (callFun (EVar "Cons") [EVar "n", callFun (EVar "range") [ESub (EVar "n") (EIntLit 1)]])
  ) $
  makeFun ("sum", TInt) [("l", TData "List")]
  (
    ECase (EVar "l")
    [ (PData "Cons" [PVar "x", PVar "xs"],
       EAdd (EVar "x") (callFun (EVar "sum") [EVar "xs"]))
    , (PData "Nil" [],
       EIntLit 0)
    ]
  ) $
  ELet ("l", callFun (EVar "range") [EIntLit 100]) $
  callFun (EVar "sum") [EVar "l"]

test_data :: [(Expr, Maybe Type, AST.Result)]
test_data = 
  [
    (EBoolLit True, Just TBool, RBool True),
    (EBoolLit False, Just TBool, RBool False),
    (EIntLit 1, Just TInt, RInt 1),
    (EIntLit (-1), Just TInt, RInt (-1)),
    (ECharLit 'a', Just TChar, RChar 'a'),
    (ECharLit '1', Just TChar, RChar '1'),
    (ENot $ ECharLit '1', Nothing, RInvalid),
    (ENot $ EBoolLit True, Just TBool, RBool False),
    (ENot $ EBoolLit False, Just TBool, RBool True),
    (EAnd (EBoolLit True) (EBoolLit False), Just TBool, RBool False),
    (EAnd (EBoolLit False) test_stuck, Just TBool, RBool False), -- 短路求值
    (EOr (EBoolLit True) (EBoolLit False), Just TBool, RBool True),
    (EOr (EBoolLit True) test_stuck, Just TBool, RBool True), -- 短路求值
    (EAdd (EIntLit 1) (EIntLit 2), Just TInt, RInt 3),
    (ESub (EIntLit 1) (EIntLit 2), Just TInt, RInt (-1)),
    (EMul (EIntLit 1) (EIntLit 2), Just TInt, RInt 2),
    (EDiv (EIntLit 10) (EIntLit 2), Just TInt, RInt 5),
    (EEq (EIntLit 1) (EIntLit 2), Just TBool, RBool False),
    (EEq (EBoolLit True) (EBoolLit True), Just TBool, RBool True),
    (EEq (ECharLit 'a') (ECharLit 'b'), Just TBool, RBool False),
    (ENeq (EIntLit 1) (EIntLit 2), Just TBool, RBool True),
    (ENeq (EBoolLit True) (EBoolLit True), Just TBool, RBool False),
    (ENeq (ECharLit 'a') (ECharLit 'b'), Just TBool, RBool True),
    (ELt (EIntLit 1) (EIntLit 2), Just TBool, RBool True),
    (ELt (EIntLit 2) (EIntLit 1), Just TBool, RBool False),
    (EGt (EIntLit 1) (EIntLit 2), Just TBool, RBool False),
    (EGt (EIntLit 2) (EIntLit 1), Just TBool, RBool True),
    (ELe (EIntLit 1) (EIntLit 2), Just TBool, RBool True),
    (ELe (EIntLit 2) (EIntLit 1), Just TBool, RBool False),
    (ELe (EIntLit 2) (EIntLit 2), Just TBool, RBool True),
    (EGe (ECharLit '1') (ECharLit '2'), Just TBool, RBool False),
    (EGe (ECharLit '2') (ECharLit '1'), Just TBool, RBool True),
    (EGe (ECharLit '2') (ECharLit '2'), Just TBool, RBool True),
    (EIf (EBoolLit True) (EIntLit 1) (EIntLit 2), Just TInt, RInt 1),
    (EIf (EBoolLit False) (EIntLit 1) (EIntLit 2), Just TInt, RInt 2),
    (EIf (EBoolLit True) (EBoolLit False) (test_stuck), Just TBool, RBool False),
    (EIf (EBoolLit False) (test_stuck) (EBoolLit False), Just TBool, RBool False),
    (ELambda ("x",TBool) (ENot (EVar "x")), Just $ TArrow TBool TBool, RInvalid),
    (ELet ("a", (EIntLit 1)) (EAdd (EVar "a") (EIntLit 1)), Just TInt, RInt 2),
    (EApply (ELambda ("x",TBool) (ENot (EVar "x"))) (EBoolLit True),Just TBool, RBool False),
    (EApply (ELambda ("x",TInt) (EAdd (EVar "x") (EIntLit 1))) (EIntLit 1),Just TInt, RInt 2),
    (test_fbi, Just TInt, RInt 89),
    (ECase (EIntLit 3) [(PIntLit 2, EIntLit 1), (PVar "x", EAdd (EVar "x") (EIntLit 1))], Just TInt, RInt 4)
  ]

(programs, expected_types, expected_values) = unzip3 test_data

func (eachT, program) = case eachT of 
  Just x -> evalValue (Program [] program) -- well typed, 求值
  Nothing -> RInvalid

types = map evalType $ map (\x -> Program [] x) programs
values = map func $ zip types programs

main :: IO ()
main = quickCheck $ values == expected_values && types == expected_types
