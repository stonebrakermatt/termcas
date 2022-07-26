{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Utilities for working with dependencies -}
module ExpData.Dependency.Utils where
import Data.List as L
import qualified ExpData.Dependency.Type as D
import qualified ExpData.Expression.Type as E





get_dependencies :: E.Expression -> [D.ExpressionDependency]
get_dependencies (E.Negate e) = get_dependencies e
get_dependencies (E.Binary o e1 e2) = get_dependencies e1 `L.union` get_dependencies e2
get_dependencies (E.Parenthetical e) = get_dependencies e
get_dependencies (E.FCall f args) =
    (f, D.F (length args)) : foldr L.union [] (map get_dependencies args)
get_dependencies (E.Id x) = [(x, D.V)]
get_dependencies (E.Num n) = []
