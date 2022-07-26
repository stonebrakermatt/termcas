{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 - 
 - File for lexing user input -}
module IO.Utils.Lexer where
import qualified IO.Utils.Capture as C
import qualified IO.Utils.Regex.GrammarRegexes as G
import qualified IO.Utils.Regex.Type as R
import qualified IO.Utils.Token as T





{- Lexer type for simplifying type signatures -}
type Lexer a = [a] -> [T.InputToken]

{- Types for matching to simplify type signatures -}
type MatchContext a = [(R.Regex a, [a] -> T.InputToken)]
type MatchResult a = ([a], T.InputToken)



{- A type for storing the list of pairs of regexes 
 - and matching constructors -}
match_context :: MatchContext Char
match_context = zip G.language_regexes T.token_constructors

{- Matches a single token, returning the token
 - and the remaining input to be lexed -}
match :: [Char] -> MatchResult Char
match user_input =
    let match' user_input [] = (user_input, T.SpaceToken "")
        match' user_input ((re, tokenizer) : match_context) =
            case user_input `C.capture` re of
                Nothing -> match' user_input match_context
                Just (tok, input_tail) -> case tokenizer tok of
                    T.NumLiteralToken n -> case match' user_input match_context of
                        (user_input', T.SpaceToken "") -> (user_input', T.SpaceToken "")
                        (input_tail', tok') -> 
                            if length input_tail > length input_tail'
                                then (input_tail', tok')
                                else (input_tail, T.NumLiteralToken n)
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
remove_spaces :: Lexer T.InputToken
remove_spaces revtokens = 
    let remove_spaces' [] tokens = tokens
        remove_spaces' (tok : revtokens) tokens = case tok of
            T.SpaceToken _ -> remove_spaces' revtokens tokens
            _ -> remove_spaces' revtokens (tok : tokens)
    in remove_spaces' revtokens []



{- Lexes input and removes spaces -}
lex :: [Char] -> [T.InputToken]
lex = remove_spaces . lex_input
