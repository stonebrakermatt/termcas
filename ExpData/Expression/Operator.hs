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
    deriving (Read, Eq)



{- Pairs of Op constructors and corresponding strings -}
get_op :: [Char] -> Op
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
operator_precedence Mod = 1
operator_precedence Choose = 1
operator_precedence Permute = 1
operator_precedence Plus = 2
operator_precedence Minus = 2
operator_precedence Times = 3
operator_precedence DivBy = 3
operator_precedence Power = 4



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
