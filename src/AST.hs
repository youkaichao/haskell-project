-- |Abstract Syntax Tree
module AST
  ( Type (..)
  , Pattern (..)
  , Expr (..)
  , ADT (..)
  , Program (..)
  , Result (..) ) where


-- |类型(type)，包括基本数据类型、函数类型以及代数数据类型。
data Type
  
  = TBool
  -- ^布尔类型的构造函数。

  | TInt
  -- ^有限精度整数类型的构造函数。

  | TChar
  -- ^字符类型的构造函数。

  | TArrow Type Type
  -- ^箭头类型的构造函数，这是函数具有的类型，第一个 @Type@ 表示函数的参数类型，第二个 @Type@ 表示函数的返回值类型。

  | TData String
  -- ^代数数据类型的构造函数，其中 @String@ 是该类型的名称。

  deriving (Show, Eq)


-- |模式(pattern)，用于模式匹配(pattern matching)。
data Pattern

  = PBoolLit Bool
  -- ^布尔字面量模式。

  | PIntLit Int
  -- ^有限精度整数字面量模式。

  | PCharLit Char
  -- ^字符字面量模式。

  | PVar String
  -- ^变量模式。

  | PData String [Pattern]
  -- ^代数数据类型模式，其中 @String@ 是某个代数数据类型的构造函数， @[Pattern]@ 是该构造函数的参数。

  deriving (Show, Eq)


-- |表达式(expression)，包括基本数据类型字面量、基本数据类型之间的运算、条件语句、 let-In 表达式、 lambda 表达式、函数定义、代数数据类型定义，每一个合法的表达式都有一个类型和一个值，类型应为一个 @Type@ 类型的变量。
data Expr
  
  = EBoolLit Bool
  -- ^布尔类型字面量(literal)。
  
  | EIntLit Int
  -- ^有限精度整数类型字面量。
  
  | ECharLit Char
  -- ^字符类型字面量。

  | ENot Expr
  -- ^逻辑取反表达式，其中 @Expr@ 是一个类型为 @TBool@ 的表达式，该表达式的类型为 @TBool@，值为 @Expr@ 的值取反。
  
  | EAnd Expr Expr
  -- ^逻辑且表达式，其中两个 @Expr@ 都为类型为 @TBool@ 的表达式，该表达式的类型为 @TBool@，值为两个 @Expr@ 的值的逻辑且，具有短路特性，即第一个 @Expr@ 值为 @False@ 时，不求第二个 @Expr@ 的值，直接返回 @False@ 。
  
  | EOr Expr Expr
  -- ^逻辑或表达式，其中两个 @Expr@ 都为类型为 @TBool@ 的表达式，该表达式的类型为 @TBool@，值为两个 @Expr@ 的值的逻辑或，具有短路特性，即第一个 @Expr@ 值为 @True@ 时，不求第二个 @Expr@ 的值，直接返回 @True@ 。

  | EAdd Expr Expr
  -- ^有限精度整数加法表达式，其中两个 @Expr@ 都为类型为 @TInt@ 的表达式，该表达式的类型为 @TInt@，值为两个 @Expr@ 的值的和。

  | ESub Expr Expr
  -- ^有限精度整数减法表达式，其中两个 @Expr@ 都为类型为 @TInt@ 的表达式，该表达式的类型为 @TInt@，值为第一个 @Expr@ 的值减去第二个 @Expr@ 的值。

  | EMul Expr Expr
  -- ^有限精度整数乘法表达式，其中两个 @Expr@ 都为类型为 @TInt@ 的表达式，该表达式的类型为 @TInt@，值为两个 @Expr@ 的值的乘积。

  | EDiv Expr Expr
  -- ^有限精度整数除法表达式，其中两个 @Expr@ 都为类型为 @TInt@ 的表达式，该表达式的类型为 @TInt@，值为第一个 @Expr@ 的值除以第二个 @Expr@ 的值。

  | EEq Expr Expr
  -- ^布尔、有限精度整数、字符判等表达式，其中两个 @Expr@ 的类型相同，且为 @TBool@ 、 @TInt@ 、 @TChar@ 三者之一，该表达式的类型为 @TBool@，当两个 @Expr@ 的值相等时，值为 @True@ ，否则为 @False@ 。

  | ENeq Expr Expr
  -- ^布尔、有限精度整数、字符不等表达式，其中两个 @Expr@ 的类型相同，且为 @TBool@ 、 @TInt@ 、 @TChar@ 三者之一，该表达式的类型为 @TBool@，当两个 @Expr@ 的值相等时，值为 @False@ ，否则为 @True@ 。
  
  | ELt Expr Expr
  -- ^小于比较表达式，其中两个 @Expr@ 的类型相同，且为 @TInt@ 、 @TChar@ 两者之一，该表达式的类型为 @TBool@，当第一个 @Expr@ 的值小于第二个 @Expr@ 的值时值为 @True@ ，否则为 @False@ 。
  
  | EGt Expr Expr
  -- ^大于比较表达式，其中两个 @Expr@ 的类型相同，且为 @TInt@ 、 @TChar@ 两者之一，该表达式的类型为 @TBool@，当第一个 @Expr@ 的值大于第二个 @Expr@ 的值时值为 @True@ ，否则为 @False@ 。
  
  | ELe Expr Expr
  -- ^小于等于比较表达式，其中两个 @Expr@ 的类型相同，且为 @TInt@ 、 @TChar@ 两者之一，该表达式的类型为 @TBool@，当第一个 @Expr@ 的值小于等于第二个 @Expr@ 的值时值为 @True@ ，否则为 @False@ 。

  | EGe Expr Expr
  -- ^大于等于比较表达式，其中两个 @Expr@ 的类型相同，且为 @TInt@ 、 @TChar@ 两者之一，该表达式的类型为 @TBool@，当第一个 @Expr@ 的值大于等于第二个 @Expr@ 的值时值为 @True@ ，否则为 @False@ 。

  | EIf Expr Expr Expr
  -- ^条件表达式，其中第一个 @Expr@ 是条件，其类型为 @TBool@ ，第二个 @Expr@ 是 @then@ 分支，第三个 @Expr@ 是 @else@ 分支，两个分支的类型必须相同，该表达式的类型与两个分支的类型相同，当条件为 @True@ 时，该表达式的值为 @then@ 分支的值，否则为 @else@ 分支的值。该表达式应当实现成非严格的，即当条件为 @True@ 时，只对 @then@ 分支求值，当条件为 @False@ 时，只对 @else@ 分支求值。

  | ELambda (String, Type) Expr
  -- ^ @lambda@ 表达式，其中 @(String, Type)@ 分别表示该 @lambda@ 表达式的参数名和参数类型， @Expr@ 是函数体。该表达式的类型为 @TArrow T0 T1@ ，其中 @T0@ 是参数类型， @T1@ 是返回值类型，即 @Expr@ 的类型。

  | ELet (String, Expr) Expr
  -- ^ @Let In@ 表达式，其中 @(String, Expr)@ 表示创建一个从 @String@ 到 @Expr@ 的绑定， @Expr@ 是该绑定的作用域，该表达式的类型为 @Expr@ 的类型，值为 @Expr@ 的值，其中第一个 @Expr@ 中不可以出现对于该 @String@ 的引用，而第二个 @Expr@ 中可以出现对于该 @String@ 的引用。

  | ELetRec String (String, Type) (Expr, Type) Expr
  -- ^针对函数定义的 @Let In@ 表达式，其中第一个 @String@ 表示函数名，第二个 @(String, Type)@ 表示参数名和参数类型，第三个 @(Expr, Type)@ 表示函数体和返回值类型，该表达式会创建一个从函数名到函数体的绑定，这个绑定值的类型为 @TArrow T0 T1@ ，其中 @T0@ 是参数类型、 @T1@ 是返回值类型， @Expr@ 是该绑定的作用域，该表达式与组合 @Let In@ 表达式与 @lambda@ 表达式的区别在于，函数体 @Expr@ 中允许引用该函数。该表达式的值为第二个 @Expr@ 的值，类型为第二个 @Expr@ 的类型。

  | EVar String
  -- ^变量表达式，类型为 @String@ 对应的变量的类型，值为 @String@ 对应的变量的类型， @String@ 到变量的绑定关系由 @Let@ 系列语句创建。

  | EApply Expr Expr
  -- ^函数应用表达式，其中第一个 @Expr@ 的类型为函数类型，即某个 @TArrow T0 T1@ ，第二个 @Expr@ 的类型为 @T0@ ，该表达式的类型为 @T1@ ，值为将第一个 @Expr@ 的值应用到第二个 @Expr@ 的值上得到的值。如果实现了代数数据类型的支持，第一个 @Expr@ 也可以是代数数据类型的构造函数。

  | ECase Expr [(Pattern, Expr)]
  -- ^模式匹配表达式，第一个 @Expr@ 的类型为 @T@ ，之后的 @[(Pattern, Expr)]@ 表示若干个模式-表达式对，这里所有的表达式的类型必须相同。该表达式的类型为模式-表达式对中表达式的类型，值为第一个匹配上的模式对应的表达式的值。

  deriving (Show, Eq)


-- |代数数据类型定义，该代数数据类型的名称为第一个 @String@ ，该代数数据类型的构造函数们用 @[(String, [Type])]@ 表达，每对 @(String, [Type])@ 表示一个名为 @String@ 的接收 @[Type]@ 作为参数的构造函数。
data ADT = ADT String [(String, [Type])] deriving (Show, Eq)


-- |程序(Program)，一个 @Program@ 由代数数据类型的定义 @[ADT]@ 以及程序体 @Expr@ 构成。
data Program = Program [ADT] Expr deriving (Show, Eq)


-- |求值结果，用于 @evalValue@ 的返回值，只需要实现返回布尔类型、有限精度整数类型、字符类型，其余的求值结果以及发生错误的求值过程都返回 @Invalid@
data Result

  = RBool Bool
  -- ^布尔类型求值结果。
  
  | RInt Int
  -- ^有限精度整数类型求值结果。
  
  | RChar Char
  -- ^字符类型求值结果。
  
  | RInvalid
  -- ^不合法的求值结果，包括求值发生错误以及非布尔类型、有限精度整数类型、字符类型的求值结果。

  deriving (Show, Eq)
