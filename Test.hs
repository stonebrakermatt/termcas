{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Test file -}
module Main where

{- System imports -}
import System.Environment
import System.IO

{- IO imports -}
import qualified IO.Command.Type as Com
import qualified IO.Command.Handlers as Handlers
import qualified IO.Utils.Regex.Type as Regex
import qualified IO.Utils.Regex.Grammar as Grammar
import qualified IO.Utils.Regex.Keywords as Keyword
import qualified IO.Utils.Lexer as Lexer
import qualified IO.Utils.Token as Token
import qualified IO.Parser as Parser

{- ExpData imports -}
import qualified ExpData.Expression.Type as Exp
import qualified ExpData.Expression.Utils as ExpUtils
import qualified ExpData.Dependency.Type as Dep
import qualified ExpData.Dependency.Utils as DepUtils
import qualified ExpData.Context.Type as Con
import qualified ExpData.Context.Utils as ConUtils

input1 = "x = 2 + f(c,y)/4"
input2 = "(((a)))"
{- Main function -}
main = do
    args <- getArgs
    sequence_ (map putStrLn args)
    case Parser.parse input1 of
        Left (Com.AssignExp e1 e2) -> 
            let a = ExpUtils.substitute_function "f" [Exp.Id "z1", Exp.Id "z2"] (Exp.Binary Exp.Plus (Exp.Id "z1") (Exp.Id "z2")) e2
            in do
                print a 
                print $ ExpUtils.remove_parens a
    case Parser.parse input2 of
        Left (Com.EvalExp e) -> 
            let a = ExpUtils.remove_parens e
            in print a