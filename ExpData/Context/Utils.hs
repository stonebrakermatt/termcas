{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Utilities for working with context -}
module ExpData.Context.Utils where
import Data.List as List
import Data.Map as Map
import qualified ExpData.Context.Type as Con
import qualified ExpData.Expression.Type as Exp
import qualified ExpData.Expression.Utils as ExpUtils
import qualified IO.Utils.Regex.Keywords as Keywords





{- Simple helper functions -}
create_context_entry :: Exp.Expression -> Exp.Expression -> Con.ContextEntry
create_context_entry e1 e2 = Left (e1, e2)
create_context_entry_set :: Exp.Set -> Exp.Set -> Con.ContextEntry
create_context_entry_set s1 s2 = Right (s1, s2)
create_context_key :: Exp.Expression -> Maybe Con.Dependency
create_context_key (Exp.Id x) = Just (x, Con.V)
create_context_key (Exp.FCall f args)
    | length (List.filter (\arg -> case arg of Exp.Id x -> True ; _ -> False) args) == length args =
        Just (f, Con.F (length args))
    | otherwise = Nothing
create_context_key _ = Nothing
create_context_key_set :: Exp.Set -> Maybe Con.Dependency
create_context_key_set (Exp.SetId sid) = Just (sid, Con.S)
create_context_key_set _ = Nothing

{- Insert a new entry into the context -}
context_insert :: Con.Context -> (Con.Dependency, Con.ContextEntry) -> Con.Context
context_insert context (key, entry) = Map.insert key entry context

{- Applies context to an expression -}
apply :: Exp.Expression -> Con.Context -> Exp.Expression
apply expr context =
    let deps = get_dependencies expr 
    in 
        let apply' [] expr context = expr
            apply' (d : deps) expr context = case Map.lookup d context of
                Nothing -> apply' deps expr context
                Just (Left (Exp.Id i, e)) -> apply' deps (ExpUtils.substitute i e expr) context
                Just (Left (Exp.FCall f args, e)) -> apply' deps (ExpUtils.substitute_function f args e expr) context
        in apply' deps expr context

apply_all :: Exp.Expression -> Int -> Con.Context -> Exp.Expression
apply_all expr 0 context = expr
apply_all expr n context = apply_all (apply expr context) (n - 1) context

{- Function to get the dependencies of an expression -}
get_dependencies :: Exp.Expression -> [Con.Dependency]
get_dependencies (Exp.Factorial e) = get_dependencies e
get_dependencies (Exp.Not e) = get_dependencies e
get_dependencies (Exp.Negate e) = get_dependencies e
get_dependencies (Exp.Binary o e1 e2) = get_dependencies e1 `List.union` get_dependencies e2
get_dependencies (Exp.Parenthetical e) = get_dependencies e
get_dependencies (Exp.FCall f args) =
    (f, Con.F (length args)) : List.foldr List.union [] (List.map get_dependencies args)
get_dependencies (Exp.Id x) = if head x == '_'
    then []
    else [(x, Con.V)]
get_dependencies (Exp.Num n) = []
get_dependencies (Exp.Boolean b) = []

{- Builds a dependency tree  -}
dep_to_tree :: Con.Dependency -> Con.Context -> [Con.Dependency] -> Con.DependencyTree
dep_to_tree d context forbidden = case Map.lookup d context of 
    Just (Left (e1, e2)) -> case get_dependencies e2 of
        [] -> if d `elem` forbidden
            then Con.Empty
            else Con.Leaf d
        deps -> Con.Branch d (List.map (\x -> dep_to_tree x context (d : forbidden)) deps)
    _ -> Con.Empty
get_dep_tree :: Exp.Expression -> Con.Context -> Con.DependencyTree
get_dep_tree expr context = case get_dependencies expr of
    [] -> Con.Empty
    (d : deps) -> Con.Branch ("_expr", Con.V) (List.map (\x -> dep_to_tree x context []) (d : deps))

depth :: Con.DependencyTree -> Int
depth (Con.Empty) = 0
depth (Con.Leaf l) = 1
depth (Con.Branch b deps) = 1 + List.foldr max 0 (List.map (\x -> depth x) deps)

{- Checks if a dependency tree contains a given dependency -}
tree_contains :: Con.DependencyTree -> Con.Dependency -> Bool
tree_contains Con.Empty  d = False
tree_contains (Con.Leaf l) d = l == d
tree_contains (Con.Branch b deps) d
    | b == d = True
    | otherwise = List.foldr (||) False (List.map (\x -> x `tree_contains` d) deps)
