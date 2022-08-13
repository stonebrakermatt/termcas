{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Main file -}
module Main where
import System.Environment
import System.IO

{- IO imports -}
import qualified IO.Command.Type as Com
import qualified IO.Command.Handlers as Handlers
import qualified IO.Parser as Parser

{- ExpData imports -}
import qualified ExpData.Context.Type as Con
import qualified ExpData.Context.Utils as ConUtils
import qualified ExpData.Dependency.Utils as DepUtils
import qualified ExpData.Expression.Type as Exp





{- Calls the main loop with a turn number of zero
 - and empty context -}
main = do
    args <- getArgs
    sequence_ (map putStrLn args)
    sequence_ welcome_dialog
    repl 0 Con.empty_context



{- Welcome dialog -}
welcome_dialog :: [IO ()]
welcome_dialog =
    [ putStrLn ""
    , putStrLn "TermCAS v.0.1.0"
    , putStrLn "For help, type \\help" ]

{- Helper for spacing the prompt -}
prompt_spaces :: Int -> [Char]
prompt_spaces n = if n > 9999999
    then " "
    else take (8 - length (show n)) (repeat ' ')

{- Main program loop -}
repl :: Int -> Con.Context -> IO ()
repl n context = do
    putStrLn $ ""
    putStr $ "=> "
    hFlush stdout
    str <- getLine
    case Parser.parse str of 
        Left cmd -> case cmd of 
            Com.AssignExp e1 e2 -> case e1 of 
                Exp.FCall f args -> case Handlers.handle_assign_function f args e2 context of 
                    Handlers.AssignSuccess (key, value) -> case value of 
                        Left (k, v) -> do
                            putStrLn $ "      Out: Successfully assigned " ++ show k ++ " to " ++ show v ++ "."
                            repl (n + 1) (context `ConUtils.context_insert` (key, value))
                        _ -> do
                            putStrLn $ "Error occurred while parsing input."
                            repl n context
                    Handlers.AssignFailure err -> do
                        putStrLn $ "Error occurred while parsing input: " ++ show err
                        repl n context
                Exp.Id i -> case Handlers.handle_assign_variable i e2 context of 
                    Handlers.AssignSuccess (key, value) -> case value of 
                        Left (k, v) -> do 
                            putStrLn $ "      Out: Successfully assigned " ++ show k ++ " to " ++ show v ++ "."
                            repl (n + 1) (context `ConUtils.context_insert` (key, value))
                        _ -> do
                            putStrLn $ "Error occurred while parsing input."
                            repl n context
                    Handlers.AssignFailure err -> do
                        putStrLn $ "Error occurred while parsing input: " ++ show err
                        repl n context
            Com.AssignSet s1 s2 -> case s1 of
                Exp.SetId sid -> case Handlers.handle_assign_set sid s2 context of
                    Handlers.AssignSuccess (key, value) -> case value of 
                        Right (k, v) -> do 
                            putStrLn $ "      Out: Successfully assigned " ++ show k ++ " to " ++ show v ++ "."
                            repl (n + 1) (context `ConUtils.context_insert` (key, value))
                        _ -> do
                            putStrLn $ "Error occurred while parsing input."
                            repl n context
                    Handlers.AssignFailure err -> do
                        putStrLn $ "Error occurred while parsing input: " ++ show err
                        repl n context
                _ -> do 
                    putStrLn $ "Error occurred while parsing input: " ++ show Handlers.LValueError
                    repl n context
            Com.EvalExp e -> do 
                print e
                print ((e `ConUtils.apply` context) `ConUtils.apply` context)
                repl n context
            Com.EvalSet set -> do
                print set
                repl n context
        _ -> repl n context