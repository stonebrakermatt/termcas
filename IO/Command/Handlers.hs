{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File for handling commands -}
module IO.Command.Handlers where
import IO.Dialog
import ExpData.Context.Type as C
import ExpData.Dependency.Type as D
import IO.Command.Type as CMD
import ExpData.Expression.Type as E
import ExpData.Context.Utils as U





{- Types for assign results -}
data AssignError
    = LValueError
    | CircularDependencyError
    | MissingDependencyError
    deriving (Show, Read)
data AssignResult
    = AssignSuccess (D.ExpressionDependency, C.ContextEntry)
    | AssignFailure AssignError
    deriving (Show, Read)

{- Creates a fake, temporary, context for function arguments so
 - as to prevent a dependency error on the rvalue expression -}
argcontext args = map (\x -> U.create_context_entry x x) args

{- Handler for valid assign operations -}
handle_assign_function :: [Char] -> [E.Expression] -> E.Expression ->  C.Context -> AssignResult
handle_assign_function f args expr context = case U.create_context_key (E.FCall f args) of
    Nothing -> AssignFailure LValueError
    Just context_key -> AssignSuccess (context_key, U.create_context_entry (E.FCall f args) expr)
handle_assign_variable :: [Char] -> E.Expression -> C.Context -> AssignResult
handle_assign_variable i expr context = case U.create_context_key (E.Id i) of
    Nothing -> AssignFailure LValueError
    Just context_key -> AssignSuccess (context_key, U.create_context_entry (E.Id i) expr)

{- Handler for builtin commands such as \about, \bindings,
 - \exit, and \help -}
handle_builtin :: CMD.Builtin -> C.Context -> IO ()
handle_builtin (CMD.About) _ = do
    sequence_ about_dialog
handle_builtin (CMD.Bindings) context = do
    putStrLn ""
    putStrLn "Current bindings:"
    sequence_ (map (putStrLn . C.show_context_entry) (C.context_elems context))
    putStrLn ""
handle_builtin (CMD.Help n) context = do
    sequence_ (help_dialog n)
handle_builtin _ _ = return ()