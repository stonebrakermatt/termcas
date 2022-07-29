{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - Test file -}
module Main where

{- System imports -}
import Prelude
import System.Environment
import System.IO

{- IO imports -}
import qualified IO.Command.Type as IOCmd
import qualified IO.Command.Handlers as IOCmdHandlers
import qualified IO.Utils.Regex.Type as IORegex
import qualified IO.Utils.Regex.GrammarRegexes as IORegexGrammar
import qualified IO.Utils.Regex.Keywords as IORegexKeyword
import qualified IO.Utils.Capture as IOCapture
import qualified IO.Utils.Lexer as IOLexer
import qualified IO.Utils.Token as IOToken
import qualified IO.Dialog as IODialog
import qualified IO.Parser as IOParser

{- ExpData imports -}
import qualified ExpData.Context.Type as ExpContext
import qualified ExpData.Context.Utils as ExpContextUtils
import qualified ExpData.Dependency.Type as ExpDependency
import qualified ExpData.Dependency.Utils as ExpDependencyUtils
import qualified ExpData.Expression.Type as ExpExpression
import qualified ExpData.Expression.Operator as ExpExpressionOp
import qualified ExpData.Expression.Utils as ExpExpressionUtils

{- Main function -}
main = do
    args <- getArgs
    sequence_ (map putStrLn args)
    print (IOLexer.lex "f(x) = x^2")
    print (IOParser.parse "f(x) = x^2")


