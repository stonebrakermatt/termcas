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
import qualified IO.Utils.Regex.Type as IORegex
import qualified IO.Utils.Regex.GrammarRegexes as IORegexGrammar
import qualified IO.Utils.Regex.Keywords as IORegexKeyword
import qualified IO.Utils.Capture as IOCapture
import qualified IO.Utils.Lexer as IOLexer
import qualified IO.Utils.Token as IOToken
import qualified IO.Dialog as IODialog
import qualified IO.Parser as IOParser

{- ExpData imports -}
import qualified ExpData.Expression.Type as ExpExpression
import qualified ExpData.Expression.Operator as ExpExpressionOp
import qualified ExpData.Expression.Utils as ExpExpressionUtils

{- Main function -}
main = do
    args <- getArgs
    sequence_ (map putStrLn args)
    print (IOParser.parse_expr (IOLexer.lex "2 = "))


