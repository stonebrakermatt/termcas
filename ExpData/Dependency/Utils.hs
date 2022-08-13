{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Utilities for working with dependencies -}
module ExpData.Dependency.Utils where
import Data.List as List
import qualified ExpData.Dependency.Type as Dep 
import qualified ExpData.Expression.Type as Exp





{- Function to get the dependencies of an expression -}
get_dependencies :: Exp.Expression -> [Dep.Dependency]
get_dependencies (Exp.Negate e) = get_dependencies e
get_dependencies (Exp.Binary o e1 e2) = get_dependencies e1 `List.union` get_dependencies e2
get_dependencies (Exp.Parenthetical e) = get_dependencies e
get_dependencies (Exp.FCall f args) =
    (f, Dep.F (length args)) : foldr List.union [] (map get_dependencies args)
get_dependencies (Exp.Id x) = if head x == '_'
    then []
    else [(x, Dep.V)]
get_dependencies (Exp.Num n) = []