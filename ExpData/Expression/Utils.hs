{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Utilities for working with expressions -}
module ExpData.Expression.Utils where
import qualified ExpData.Expression.Type as Exp
import qualified IO.Utils.Regex.Grammar as Grammar
import qualified IO.Utils.Lexer as Lexer





{- Utilities for substituting expressions for a variables -}
substitute :: [Char] -> Exp.Expression -> Exp.Expression -> Exp.Expression
substitute x e expr = case expr of
    Exp.Negate e1 -> Exp.Negate (substitute x e e1)
    Exp.Binary o e1 e2 -> Exp.Binary o (substitute x e e1) (substitute x e e2)
    Exp.Parenthetical e1 -> Exp.Parenthetical (substitute x e e1)
    Exp.Id y -> if y == x
        then e
        else Exp.Id y
    Exp.FCall f args -> Exp.FCall f (map (\e1 -> substitute x e e1) args)
    Exp.Num n -> Exp.Num n
substitute_args :: [Exp.Expression] -> [Exp.Expression] -> Exp.Expression -> Exp.Expression
substitute_args [] [] expr = expr
substitute_args (a1 : argvars) (a2 : arglist) expr = case a1 of
    Exp.Id x -> substitute_args argvars arglist (substitute x a2 expr)
    _ -> expr

{- Test if an expression is an integer -}
is_integer :: Exp.Expression -> Bool
is_integer (Exp.Num n) = case n `Lexer.capture` Grammar.regex_number_integer of
    Just _ -> True
    _ -> False
is_integer _ = False
