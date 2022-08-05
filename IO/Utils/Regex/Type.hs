{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File defining a regular expression data type
 - as well as functions for how to display and 
 - read data of this type -}
module IO.Utils.Regex.Type where





{- Regular expression data type
 - for robust pattern matching -}
data Regex
    = RegexAnd [Regex] -- Matches a list of Regexes in sequence
    | RegexOr [Regex] -- Matches any of the regexes in the list
    | RegexConst Char -- Mathces a single literal value
    | RegexWord [Char] -- Matches a string of literals in sequence
    | RegexInSet [Char] -- Matches any of a list of literal values
    | RegexNotInSet [Char] -- Inverse match on a list of literal values
    | RegexStar Regex -- Matches the regex in sequence zero or more times
    | RegexMaybe Regex -- Matches the regex zero or one times
    | RegexPlus Regex -- Matches a regex in sequence one or more tmes
    | RegexEmpty -- Matches always
    deriving (Read, Eq)



{- Helper functions for defining the show function on
 - regular expressions to make them an instance of the
 - Show typeclass. -}

{- This function escapes regex literals -}
escape_literals :: [Char] -> [Char]
escape_literals str =
    let escape_literals' [] rev = reverse rev
        escape_literals' (h : t) rev
            | h == ' ' = escape_literals' t ('s' : '\\' : rev)
            | h == '\t' = escape_literals' t ('t' : '\\' : rev)
            | h == '.' = escape_literals' t ('.' : '\\' : rev)
            | h == '(' = escape_literals' t ('(' : '\\' : rev)
            | h == ')' = escape_literals' t (')' : '\\' : rev)
            | h == '+' = escape_literals' t ('+' : '\\' : rev)
            | h == '*' = escape_literals' t ('*' : '\\' : rev)
            | h == '^' = escape_literals' t ('^' : '\\' : rev)
            | h == '[' = escape_literals' t ('[' : '\\' : rev)
            | h == ']' = escape_literals' t (']' : '\\' : rev)
            | h == '-' = escape_literals' t ('-' : '\\' : rev)
            | otherwise = escape_literals' t (h : rev)
    in escape_literals' str []

{- This function takes three arguments:
 - the list of literals to match against;
 - a boolean that is only true on first iteration
 - to print the edge case correctly; and
 - a boolean for if the match is inverted -}
show_regex_list :: [Char] -> Bool -> Bool -> [Char]
show_regex_list const_list first_iter invert = if first_iter 
    then if null const_list
        then []
        else if invert
            then "[^" ++ escape_literals [head const_list] ++ show_regex_list (tail const_list) False invert
            else '[' : escape_literals [head const_list] ++ show_regex_list (tail const_list) False invert
    else if null const_list
        then "]"
        else escape_literals [head const_list] ++ show_regex_list (tail const_list) False invert

{- This function pretty prints disjunctions -}
show_or (RegexOr []) first_iter = 
    if first_iter
        then "()"
        else ")"
show_or (RegexOr (re : re_list)) first_iter = 
    if first_iter
        then "(" ++ show re ++ "|" ++ show_or (RegexOr re_list) False
        else if null re_list
            then show re ++ show_or (RegexOr re_list) False 
            else show re ++ "|" ++ show_or (RegexOr re_list) False

{- Adds parentheses around a regular expression only if it will
 - take more than one character to print. This helps put parentheses
 - only where necessary for pretty printing -}
parens_unless_singleton :: Regex -> [Char]
parens_unless_singleton re
    | length (show re) > 1 = '(' : (show re) ++ ")"
    | otherwise = show re



{- Define how to print regular expressions. This pretty much
 - follows standard regex syntax -}
instance Show Regex where
    show (RegexAnd []) = []
    show (RegexAnd (re : re_list)) = show re ++ show (RegexAnd re_list)
    show (RegexOr []) = []
    show (RegexOr (re : re_list)) = show_or (RegexOr (re : re_list)) True
    show (RegexConst c) = escape_literals [c]
    show (RegexWord w) = escape_literals w
    show (RegexInSet []) = []
    show (RegexInSet const_list) = show_regex_list const_list True False
    show (RegexNotInSet []) = "."
    show (RegexNotInSet const_list) = show_regex_list const_list True True
    show (RegexStar re) = (parens_unless_singleton re) ++ "*" 
    show (RegexMaybe re) = (parens_unless_singleton re) ++ "?"
    show (RegexPlus re) = (parens_unless_singleton re) ++ "+"
    show RegexEmpty = ""
