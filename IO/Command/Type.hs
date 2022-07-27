{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File for handling commands -}
module IO.Command.Type where
import qualified ExpData.Expression.Type as E





{- Type for builtin backslash commands -}
data Builtin
    = About
    | Bindings
    | Exit
    | Help Int
    deriving (Read, Eq)

{- Display builtin commands -}
instance Show Builtin where
    show About = "show about dialog"
    show Bindings = "show current bindings"
    show Exit = "exit TermCAS"
    show (Help n) = "show help dialog page " ++ show n

{- Type for general commands -}
data Command
    = Eval E.Expression
    | Assign E.Expression E.Expression
    | Builtin Builtin
    deriving (Read, Eq)

{- Display commands -}
instance Show Command where
    show (Eval e) = "evaluate " ++ show e
    show (Assign e1 e2) = "assign " ++ show e1 ++ " to " ++ show e2
    show (Builtin b) = show b
