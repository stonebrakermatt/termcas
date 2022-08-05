{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File for defining valid user input
 - tokens to match while lexing -}
module IO.Utils.Token where





{- Token type for lexing user input -}
data InputToken
    = RelationToken [Char]
    | IdToken [Char]
    | OpToken [Char]
    | NumLiteralToken [Char]
    | DelimiterToken [Char]
    | SpaceToken [Char]
    | BadInputToken [Char]
    deriving (Read, Eq)



{- Token constructors. See Lexer for its use -}
token_constructors =
    [ RelationToken
    , DelimiterToken
    , OpToken
    , NumLiteralToken
    , IdToken
    , SpaceToken
    , BadInputToken ]

{- Define how tokens are printed -}
instance Show InputToken where
    show (RelationToken str) = "<rel:" ++ str ++ ">"
    show (IdToken str) = "<id:" ++ str ++ ">"
    show (OpToken str) = "<op:" ++ str ++ ">"
    show (NumLiteralToken str) = "<num:" ++ str ++ ">"
    show (DelimiterToken str) = "<del:" ++ str ++ ">"
    show (SpaceToken str) = "<sp:" ++ str ++ ">"
    show (BadInputToken str) = "<err:" ++ str ++ ">"