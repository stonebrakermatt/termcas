{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File for builtin operators -}
module ExpData.Expression.Operator where





{- Op data type for all supported binary operations -}
data Op
    = Power
    | Times
    | DivBy
    | Plus
    | Minus
    | Mod
    | Choose
    | Permute
    | Equals
    | NE
    | G
    | GE
    | L
    | LE
    | And
    | Or
    | Xor
    deriving (Read, Eq)



{- Pairs of Op constructors and corresponding strings -}
get_op :: [Char] -> Op
get_op "and" = And
get_op "or" = Or
get_op "xor" = Xor
get_op "==" = Equals
get_op "!=" = NE
get_op ">" = G
get_op ">=" = GE
get_op "<" = L
get_op "<=" = LE
get_op "mod" = Mod
get_op "choose" = Choose
get_op "permute" = Permute
get_op "+" = Plus
get_op "-" = Minus
get_op "*" = Times
get_op "/" = DivBy
get_op _ = Power

{- Defines operator precedence levels -}
operator_precedence :: Op -> Int
operator_precedence And = 1
operator_precedence Or = 1
operator_precedence Xor = 1
operator_precedence Equals = 2
operator_precedence NE = 2
operator_precedence G = 2
operator_precedence GE = 2
operator_precedence L = 2
operator_precedence LE = 2
operator_precedence Mod = 3
operator_precedence Choose = 3
operator_precedence Permute = 3
operator_precedence Plus = 4
operator_precedence Minus = 4
operator_precedence Times = 5
operator_precedence DivBy = 5
operator_precedence Power = 6



{- How to print the operations -}
instance Show Op where
    show Power = "^"
    show Times = "*"
    show DivBy = "/"
    show Plus = "+"
    show Minus = "-"
    show Mod = " mod "
    show Choose = " choose "
    show Permute = " permute "
    show Equals = " == "
    show NE = " != "
    show G = " > "
    show GE = " >= "
    show L = " < "
    show LE = " <= "
    show And = " and "
    show Or = " or "
    show Xor = " xor "