{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Utilities for working with expressions -}
module ExpData.Expression.Utils where
import Data.List
import qualified ExpData.Expression.Type as E





{- Utilities for substituting expressions for a variables -}
substitute :: [Char] -> E.Expression -> E.Expression -> E.Expression
substitute x e expr = case expr of
    E.Negate e1 -> E.Negate (substitute x e e1)
    E.Binary o e1 e2 -> E.Binary o (substitute x e e1) (substitute x e e2)
    E.Parenthetical e1 -> E.Parenthetical (substitute x e e1)
    E.Id y -> if y == x
        then e
        else E.Id y
    E.FCall f args -> E.FCall f (map (\e1 -> substitute x e e1) args)
    E.Num n -> E.Num n
substitute_args :: [E.Expression] -> [E.Expression] -> E.Expression -> E.Expression
substitute_args [] [] expr = expr
substitute_args (a1 : argvars) (a2 : arglist) expr = case a1 of
    E.Id x -> substitute_args argvars arglist (substitute x a2 expr)
    _ -> expr

get_dependencies :: E.Expression -> [E.ExpressionDependency]
get_dependencies (E.Negate e) = get_dependencies e
get_dependencies (E.Binary o e1 e2) = get_dependencies e1 `union` get_dependencies e2
get_dependencies (E.Parenthetical e) = get_dependencies e
get_dependencies (E.FCall f args) = (f, E.F 1) : foldr (++) [] (map get_dependencies args)
get_dependencies (E.Id x) = [(x, E.V)]
get_dependencies (E.Num n) = []
