{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Utilities for working with expressions -}
module ExpData.Expression.Utils where
import qualified ExpData.Expression.Type as Exp
import qualified IO.Utils.Regex.Grammar as Grammar
import qualified IO.Utils.Lexer as Lexer





{- Test if an expression is an integer -}
is_integer :: Exp.Expression -> Bool
is_integer (Exp.Num n) = case n `Lexer.capture` Grammar.regex_number_integer of
    Just _ -> True
    _ -> False
is_integer _ = False

{- Utilities for substituting expressions for a variables -}
substitute :: [Char] -> Exp.Expression -> Exp.Expression -> Exp.Expression
substitute x e expr = case expr of
    Exp.Negate e1 -> Exp.Negate (Exp.Parenthetical (substitute x e e1))
    Exp.Binary o e1 e2 -> Exp.Parenthetical (Exp.Binary o (substitute x e e1) (substitute x e e2))
    Exp.Parenthetical e1 -> Exp.Parenthetical (substitute x e e1)
    Exp.Id y -> if y == x
        then e
        else Exp.Id y
    Exp.FCall f args -> Exp.FCall f (map (\e1 -> Exp.Parenthetical (substitute x e e1)) args)
    Exp.Num n -> Exp.Num n
substitute_args :: [Exp.Expression] -> [Exp.Expression] -> Exp.Expression -> Exp.Expression
substitute_args [] [] expr = expr
substitute_args (a1 : argvars) (a2 : arglist) expr = case a1 of
    Exp.Id x -> substitute_args argvars arglist (substitute x a2 expr)
    _ -> expr
substitute_function :: [Char] -> [Exp.Expression] -> Exp.Expression -> Exp.Expression -> Exp.Expression
substitute_function f args e expr = case expr of
    Exp.Negate e1 -> Exp.Negate (Exp.Parenthetical (substitute_function f args e e1))
    Exp.Binary o e1 e2 -> Exp.Parenthetical (Exp.Binary o (substitute_function f args e e1) (substitute_function f args e e2))
    Exp.Parenthetical e1 -> Exp.Parenthetical (substitute_function f args e e1)
    Exp.Id x -> Exp.Id x
    Exp.FCall g args' -> if g == f 
        then Exp.Parenthetical (substitute_args args args' e)
        else Exp.FCall g args'
    Exp.Num n -> Exp.Num n

remove_parens :: Exp.Expression -> Exp.Expression
remove_parens expr = case expr of
    Exp.Factorial e -> Exp.Factorial $ remove_parens e
    Exp.Not e -> Exp.Not $ remove_parens e
    Exp.Negate e -> Exp.Negate $ remove_parens e
    Exp.Binary o e1 e2 -> Exp.Binary o (remove_parens e1) (remove_parens e2)
    Exp.Parenthetical e -> case e of 
        Exp.Parenthetical e' -> Exp.Parenthetical $ remove_parens e'
        Exp.FCall f args -> Exp.FCall f (map remove_parens args)
        Exp.Id x -> Exp.Id x
        Exp.Num n -> Exp.Num n
        Exp.Boolean b -> Exp.Boolean b
        e' -> Exp.Parenthetical e'
    Exp.FCall f args -> Exp.FCall f (map remove_parens args)
    Exp.Id x -> Exp.Id x
    Exp.Num n -> Exp.Num n
    Exp.Boolean b -> Exp.Boolean b

