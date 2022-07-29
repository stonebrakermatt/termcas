{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Main file -}
module Main where
import System.IO
import IO.Command.Handlers
import qualified IO.Command.Type as D
import qualified IO.Parser as P
import qualified ExpData.Context.Type as C
import qualified ExpData.Expression.Type as E
import qualified ExpData.Context.Utils as U

{- Calls the main loop with a turn number of zero
 - and empty context -}
main = repl 0 C.empty_context

{- Helper for spacing the prompt -}
prompt_spaces :: Int -> [Char]
prompt_spaces n = if n > 9999999
    then " "
    else take (8 - length (show n)) (repeat ' ')

{- Main program loop -}
repl :: Int -> C.Context -> IO ()
repl n context = do
    putStr (show n ++ prompt_spaces n ++ "=> ")
    hFlush stdout
    str <- getLine
    case P.parse str of
        Left cmd -> case cmd of
            D.Builtin b -> if b == D.Exit
                then return ()
                else do
                    handle_builtin b context
                    repl n context
            D.Assign e1 e2 -> case e1 of
                E.FCall f args -> case handle_assign_function f args e2 context of
                    AssignSuccess kvpair -> do
                        (putStrLn . show) kvpair
                        repl (n + 1) (context `U.context_insert` kvpair)
                    AssignFailure e -> do
                        (putStrLn . show) e
                        repl n context
                E.Id i -> case handle_assign_variable i e2 context of
                    AssignSuccess kvpair -> do
                        (putStrLn . show) kvpair
                        repl (n + 1) (context `U.context_insert` kvpair)
                    AssignFailure e -> do
                        (putStrLn . show) e
                        repl n context
                _ -> do
                    putStrLn ("Error occurred while parsing input: " ++ show LValueError)
                    repl n context
            _ -> do
                putStrLn ("out: " ++ show cmd)
                repl (n + 1) context
        Right err -> do
            putStrLn ("Error occurred while parsing input: " ++ show err)
            repl n context
