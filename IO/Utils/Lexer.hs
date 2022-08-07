{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 - 
 - File for lexing user input -}
module IO.Utils.Lexer (IO.Utils.Lexer.lex) where
import qualified IO.Utils.Capture as Capture
import qualified IO.Utils.Regex.GrammarRegexes as REGrammar
import qualified IO.Utils.Regex.Type as RE
import qualified IO.Utils.Token as Token





{- Lexer type for simplifying type signatures -}
type Lexer a = [a] -> [Token.InputToken]

{- Types for matching to simplify type signatures -}
type MatchContext = [(RE.Regex, [Char] -> Token.InputToken)]
type MatchResult = ([Char], Token.InputToken)



{- A type for storing the list of pairs of regexes 
 - and matching constructors -}
match_context :: MatchContext
match_context = zip REGrammar.language_regexes Token.token_constructors

{- Matches a single token, returning the token
 - and the remaining input to be lexed -}
match :: [Char] -> MatchResult
match user_input =
    let match' user_input [] = (user_input, Token.SpaceToken "")
        match' user_input ((re, tokenizer) : match_context) =
            case user_input `Capture.capture` re of
                Nothing -> match' user_input match_context
                Just (tok, input_tail) -> case tokenizer tok of
                    Token.NumLiteralToken n -> case match' user_input match_context of
                        (user_input', Token.SpaceToken "") -> (user_input', Token.SpaceToken "")
                        (input_tail', tok') -> 
                            if length input_tail > length input_tail'
                                then (input_tail', tok')
                                else (input_tail, Token.NumLiteralToken n)
                    _ -> (input_tail, tokenizer tok)
    in match' user_input match_context

{- Lexes all user input, using match -}
lex_input :: Lexer Char
lex_input user_input = 
    let lex_input' [] revtokens = revtokens
        lex_input' uinput revtokens = 
            let (input_tail, tok) = match uinput
            in lex_input' input_tail (tok : revtokens)
    in lex_input' user_input []

{- Removes spaces from lexed input -}
remove_spaces :: Lexer Token.InputToken
remove_spaces revtokens = 
    let remove_spaces' [] tokens = tokens
        remove_spaces' (tok : revtokens) tokens = case tok of
            Token.SpaceToken _ -> remove_spaces' revtokens tokens
            _ -> remove_spaces' revtokens (tok : tokens)
    in remove_spaces' revtokens []



{- Lexes input and removes spaces -}
lex :: Lexer Char
lex = remove_spaces . lex_input
