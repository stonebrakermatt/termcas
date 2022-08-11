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
import qualified IO.Utils.Regex.Type as Regex
import qualified IO.Utils.Regex.Grammar as Grammar
import qualified IO.Utils.Regex.Keywords as Keyword
import qualified IO.Utils.Lexer as Lexer
import qualified IO.Utils.Token as Token
import qualified IO.Parser as Parser

{- ExpData imports -}
import qualified ExpData.Expression.Type as Exp
import qualified ExpData.Expression.Utils as Utils

input = Lexer.lex "R = {x in R : f(x,2) < 2}"

{- Main function -}
main = do
    args <- getArgs
    sequence_ (map putStrLn args)
    print (Parser.parse_command input)