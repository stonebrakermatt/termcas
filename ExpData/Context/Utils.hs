{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Utilities for working with context -}
module ExpData.Context.Utils where
import Data.List as L
import Data.Map as M
import qualified ExpData.Context.Type as C
import qualified ExpData.Dependency.Type as D
import qualified ExpData.Expression.Type as E





{- Simple helper functions -}
create_context_entry :: E.Expression -> E.Expression -> C.ContextEntry
create_context_entry e1 e2 = (e1, e2)
create_context_key :: E.Expression -> Maybe D.ExpressionDependency
create_context_key (E.Id x) = Just (x, D.V)
create_context_key (E.FCall f args)
    | length (L.filter (\arg -> case arg of E.Id x -> True ; _ -> False) args) == length args =
        Just (f, D.F (length args))
    | otherwise = Nothing
create_context_key _ = Nothing

{- Insert a new entry into the context -}
context_insert :: C.Context -> (D.ExpressionDependency, C.ContextEntry) -> C.Context
context_insert context (key, entry) = M.insert key entry context
