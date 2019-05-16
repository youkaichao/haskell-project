import AST
import EvalValue
import EvalType
import Control.Monad.State

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
  Program [] $

  makeFun ("fbi", TInt) [("x", TInt)] (
  EIf (ELe (EVar "x") (EIntLit 1))
    (EIntLit 1)
    (EAdd
      (EApply (EVar "fbi") (ESub (EVar "x") (EIntLit 1)))
      (EApply (EVar "fbi") (ESub (EVar "x") (EIntLit 2))))
  ) $

  callFun (EVar "fbi") [EIntLit 10]

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

test_type =
  Program [] $
  ECase (EBoolLit True)
  [ (PBoolLit True, EIntLit 2)
  , (PBoolLit False, EIntLit 1)
  ]


test_bool_value =
  Program [] $
  ENot (EBoolLit False)

test_bool_type_1 =
  Program [] $
  ENot (EBoolLit False)
 
test_bool_type_2 =  
  Program [] $
  ENot (EIntLit 42)

programs = 
  [
  EBoolLit True,
  EBoolLit False,
  EIntLit 1,
  EIntLit (-1),
  EAnd (EBoolLit True) (EBoolLit False),
  EAdd (EIntLit 1) (EIntLit 2),
  EDiv (EIntLit 10) (EIntLit 2),
  EEq (EIntLit 1) (EIntLit 2),
  ENeq (EIntLit 1) (EIntLit 2),
  EIf (EBoolLit True) (EIntLit 1) (EIntLit 2),
  ELet ("a", (EIntLit 1)) (EAdd (EVar "a") (EIntLit 1)),
  EApply (ELambda ("x",TBool) (ENot (EVar "x"))) (EBoolLit True),
  EApply (ELambda ("x",TInt) (EAdd (EVar "x") (EIntLit 1))) (EIntLit 1)
  ]

expected_types = 
  [

  ]

expected_values = 
  [

  ]

func (eachT, program) = case eachT of 
  Just x -> evalValue (Program [] program) -- well typed, 求值
  Nothing -> RInvalid

types = map evalType $ map (\x -> Program [] x) programs
values = map func $ zip types programs

main :: IO ()
main = do
  putStrLn ""
  putStrLn " ---------- My Test Cases ----------"
  putStrLn (show types)
  putStrLn (show values)

  putStrLn " ---------- make `stack test` looks prettier ----------"
  print $ EvalValue.evalValue test_bool_value -- should be: RBool True
  print $ EvalType.evalType test_bool_type_1 -- should be: Just TBool
  print $ EvalType.evalType test_bool_type_2 -- should be: Nothing
  
  print $ EvalValue.evalValue test_fbi
  print $ EvalValue.evalValue test_sum3
  -- print $ EvalValue.evalValue test_adt_ctor
  -- print $ EvalValue.evalValue test_adt_case
  -- print $ EvalValue.evalProgram test_adt_list
  -- print $ EvalValue.evalProgram test_adt_list_range
  -- print $ EvalValue.evalValue test_adt_list_sum
  print $ EvalType.evalType test_fbi
  print $ EvalType.evalType test_sum3
  -- print $ EvalType.evalType test_adt_ctor
  -- print $ EvalType.evalType test_adt_case
  -- print $ EvalType.evalType test_adt_list
  -- print $ EvalType.evalType test_adt_list_range
  -- print $ EvalType.evalType test_adt_list_sum
  print $ EvalType.evalType test_type

  
