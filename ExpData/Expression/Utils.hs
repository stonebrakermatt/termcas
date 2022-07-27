{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Utilities for working with expressions -}
module ExpData.Expression.Utils where
import Data.List
import qualified ExpData.Expression.Type as E
import qualified ExpData.Dependency.Type as D





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
