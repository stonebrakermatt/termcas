{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File defining an expression type -}
module ExpData.Expression.Type where





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



{- Expression type -}
data Expression
    = Factorial Expression
    | Not Expression
    | Negate Expression
    | Binary Op Expression Expression
    | Parenthetical Expression
    | FCall [Char] [Expression]
    | Id [Char]
    | Num [Char]
    | Boolean Bool
    deriving (Read, Eq)



{- Helpers for showing args -}
get_arg_str :: [Expression] -> [[Char]]
get_arg_str exprs = map show exprs
add_commas :: [String] -> String -> String
add_commas [] revStr = reverse revStr
add_commas (s : []) revStr = reverse (reverse s ++ revStr)
add_commas (s1 : s2 : strs) revStr = add_commas (s2 : strs) ("," ++ reverse s1 ++ revStr)

{- How to print expressions -}
instance Show Expression where
    show (Factorial e) = show e ++ "!"
    show (Not e) = case e of
        Binary o e1 e2 -> "not (" ++ show e ++ ")"
        _ -> "not " ++ show e 
    show (Negate e) = case e of
        Negate e' -> show e'
        Binary o e1 e2 -> "-(" ++ show e ++ ")"
        _ -> "-" ++ show e
    show (Binary o e1 e2) = show e1 ++ show o ++ show e2
    show (Parenthetical e) = "(" ++ show e ++ ")"
    show (FCall f args) = f ++ "(" ++ (add_commas (get_arg_str args) []) ++ ")"
    show (Id i) = i
    show (Num n) = n
    show (Boolean bool) = show bool



{- Type for sets -}
data Set 
    = SetId [Char]
    | Set [[Char]] Set [Expression]
    | STimes Set Set
    | SExp Set Int
    deriving (Read, Eq)

{- Helper functions for showing sets -}
show_params :: [[Char]] -> [Char] -> Bool -> [Char]
show_params [] str first_iter = str ++ ")"
show_params (p : params) str first_iter
    | first_iter = if length (p : params) == 1
        then p
        else show_params params ('(' : p) False
    | otherwise = show_params params (str ++ (',' : p)) False 
show_conds :: [Expression] -> [Char] -> Bool -> [Char]
show_conds [] str first_iter = str
show_conds (c : conds) str first_iter 
    | first_iter = show_conds conds (str ++ show c) False
    | otherwise = show_conds conds (str ++ ", " ++ show c) False



{- Showing sets -}
instance Show Set where
    show (SetId sid) = sid
    show (Set params base []) = show base
    show (Set (p : params) base (c : conds))
        = "{" 
        ++ show_params (p : params) [] True
        ++ " in " ++ show base ++ " : "
        ++ show_conds (c : conds) [] True
        ++ "}"
    show (STimes s1 s2) = show s1 ++ "*" ++ show s2
    show (SExp s n) = show s ++ "^" ++ show n