{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File for handling commands -}
module IO.Command.Handlers where

{- IO imports -}
import IO.Dialog
import IO.Command.Type as Com
import IO.Utils.Regex.Keywords as Keywords

{- ExpData imports -}
import ExpData.Context.Type as Con
import ExpData.Context.Utils as ConUtils
import ExpData.Dependency.Type as Dep
import ExpData.Dependency.Utils as DepUtils
import ExpData.Expression.Type as Exp
import ExpData.Expression.Utils as ExpUtils





{- Types for assign results -}
data AssignError
    = LValueError
    | CircularDependencyError
    | MissingDependencyError
    | ReservedWordError
    | ReindexError
    | DuplicateArgumentError
    deriving (Read)
data AssignResult
    = AssignSuccess (Dep.Dependency, Con.ContextEntry)
    | AssignFailure AssignError
    deriving (Show, Read)

{- Pretty print errors -}
instance Show AssignError where
    show LValueError = "Err: Invalid lvalue."
    show CircularDependencyError = "Err: Circular dependencies."
    show MissingDependencyError = "Err: Not all dependencies satisfied."
    show ReservedWordError = "Err: Reserved words cannot be variable names."
    show ReindexError = "Err: Error reindexing the function."
    show DuplicateArgumentError = "Err: Duplicate arguments."



{- Reindex functions to use reserved words as parameters -}
reindex :: [Exp.Expression] -> Exp.Expression -> Maybe ([Exp.Expression], Exp.Expression)
reindex args expr =
    let reindex' n [] expr revargs = Just (reverse revargs, expr)
        reindex' n (a : args) expr revargs = case a of 
            Exp.Id i -> let index = Exp.Id ("_x" ++ show n) 
                in reindex' (n + 1) args (ExpUtils.substitute i index expr) (index : revargs)
    in reindex' 0 args expr []

{- Test for duplicate arguments -}
contains_duplicates :: [Exp.Expression] -> Bool
contains_duplicates [] = False
contains_duplicates (a : args) = if a `elem` args
    then True
    else contains_duplicates args

{- Handlers for valid assign operations -}
handle_assign_variable :: [Char] -> Exp.Expression -> Con.Context -> AssignResult
handle_assign_variable i expr context = if i `elem` Keywords.constants
    then AssignFailure ReservedWordError
    else case ConUtils.create_context_key (Exp.Id i) of
        Nothing -> AssignFailure LValueError
        Just context_key -> AssignSuccess (context_key, ConUtils.create_context_entry (Exp.Id i) expr)
handle_assign_function :: [Char] -> [Exp.Expression] -> Exp.Expression ->  Con.Context -> AssignResult
handle_assign_function f args expr context = if f `elem` Keywords.special_funcs
    then AssignFailure ReservedWordError
    else if contains_duplicates args 
        then AssignFailure DuplicateArgumentError
        else case reindex args expr of
            Just (new_args, new_expr) -> case ConUtils.create_context_key (Exp.FCall f new_args) of
                Nothing -> AssignFailure LValueError
                Just context_key -> AssignSuccess (context_key, ConUtils.create_context_entry (Exp.FCall f new_args) new_expr)
            Nothing -> AssignFailure ReindexError
handle_assign_set :: [Char] -> Exp.Set -> Con.Context -> AssignResult
handle_assign_set s set context = case ConUtils.create_context_key_set (Exp.SetId s) of
    Nothing -> AssignFailure LValueError
    Just context_key -> AssignSuccess (context_key, ConUtils.create_context_entry_set (Exp.SetId s) set)



{- Handler for builtin commands such as \about, \bindings,
 - \exit, and \help -}
handle_builtin :: Com.Builtin -> Con.Context -> IO ()
handle_builtin (Com.About) _ = do
    sequence_ about_dialog
handle_builtin (Com.Bindings) context = do
    putStrLn ""
    putStrLn "Current bindings:"
    sequence_ (map (putStrLn . Con.show_context_entry) (Con.context_elems context))
handle_builtin (Com.Help n) context = do
    sequence_ (help_dialog n)
handle_builtin _ _ = return ()