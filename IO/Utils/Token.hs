{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File for defining valid user input
 - tokens to match while lexing -}
module IO.Utils.Token where





{- Token type for lexing user input -}
data InputToken
    = CommandToken [Char]
    | KeywordToken [Char]
    | OpToken [Char]
    | AssignToken [Char]
    | DelimiterToken [Char]
    | NumLiteralToken [Char]
    | BooleanToken [Char]
    | IdToken [Char]
    | SetIdToken [Char]
    | SpaceToken [Char]
    | BadInputToken [Char]
    deriving (Read, Eq)



{- Token constructors. See Lexer for its use -}
token_constructors =
    [ CommandToken
    , KeywordToken
    , OpToken
    , AssignToken
    , DelimiterToken
    , NumLiteralToken
    , BooleanToken
    , IdToken
    , SetIdToken
    , SpaceToken
    , BadInputToken ]

{- Define how tokens are printed -}
instance Show InputToken where
    show (CommandToken str) = "<com:\"" ++ str ++ "\">"
    show (KeywordToken str) = "<key:\"" ++ str ++ "\">"
    show (OpToken str) = "<op:\"" ++ str ++ "\">"
    show (AssignToken str) = "<ass:\"" ++ str ++ "\">"
    show (DelimiterToken str) = "<del:\"" ++ str ++ "\">"
    show (NumLiteralToken str) = "<num:\"" ++ str ++ "\">"
    show (BooleanToken str) = "<boo:\"" ++ str ++ "\">"
    show (IdToken str) = "<id:\"" ++ str ++ "\">"
    show (SetIdToken str) = "<sid:\"" ++ str ++ "\">"
    show (SpaceToken str) = "<sp:\"" ++ str ++ "\">"
    show (BadInputToken str) = "<err:\"" ++ str ++ "\">"