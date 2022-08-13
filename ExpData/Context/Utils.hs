{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Utilities for working with context -}
module ExpData.Context.Utils where
import Data.List as List
import Data.Map as Map
import qualified ExpData.Context.Type as Con
import qualified ExpData.Dependency.Type as Dep
import qualified ExpData.Dependency.Utils as DepUtils
import qualified ExpData.Expression.Type as Exp
import qualified ExpData.Expression.Utils as ExpUtils





{- Simple helper functions -}
create_context_entry :: Exp.Expression -> Exp.Expression -> Con.ContextEntry
create_context_entry e1 e2 = Left (e1, e2)
create_context_entry_set :: Exp.Set -> Exp.Set -> Con.ContextEntry
create_context_entry_set s1 s2 = Right (s1, s2)
create_context_key :: Exp.Expression -> Maybe Dep.Dependency
create_context_key (Exp.Id x) = Just (x, Dep.V)
create_context_key (Exp.FCall f args)
    | length (List.filter (\arg -> case arg of Exp.Id x -> True ; _ -> False) args) == length args =
        Just (f, Dep.F (length args))
    | otherwise = Nothing
create_context_key _ = Nothing
create_context_key_set :: Exp.Set -> Maybe Dep.Dependency
create_context_key_set (Exp.SetId sid) = Just (sid, Dep.S)
create_context_key_set _ = Nothing

{- Insert a new entry into the context -}
context_insert :: Con.Context -> (Dep.Dependency, Con.ContextEntry) -> Con.Context
context_insert context (key, entry) = Map.insert key entry context

{- Returns an unsatisfied dependency or Nothing if there are none -}
satisfies_dependencies :: Con.Context -> [Dep.Dependency] -> Maybe Dep.Dependency
context `satisfies_dependencies` [] = Nothing
context `satisfies_dependencies` (d : dependencies) = case Map.lookup d context of
    Nothing -> Just d
    Just result -> case result of 
        Left (e1, e2) -> let additional_deps = DepUtils.get_dependencies e2 
            in context `satisfies_dependencies` (additional_deps ++ dependencies)
        Right (s1, s2) -> Nothing

{- Applies context to an expression -}
apply :: Exp.Expression -> Con.Context -> Exp.Expression
apply expr context =
    let deps = DepUtils.get_dependencies expr 
    in 
        let apply' [] expr context = expr
            apply' (d : deps) expr context = case Map.lookup d context of
                Nothing -> apply' deps expr context
                Just (Left (Exp.Id i, e)) -> apply' deps (ExpUtils.substitute i e expr) context
                Just (Left (Exp.FCall f args, e)) -> apply' deps (ExpUtils.substitute_function f args e expr) context
        in apply' deps expr context
    