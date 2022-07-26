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
data Regex a
    = RegexAnd [Regex a] -- Matches a list of Regexes in sequence
    | RegexOr [Regex a] -- Matches any of the regexes in the list
    | RegexConst a -- Mathces a single literal value
    | RegexWord [a] -- Matches a string of literals in sequence
    | RegexInSet [a] -- Matches any of a list of literal values
    | RegexNotInSet [a] -- Inverse match on a list of literal values
    | RegexStar (Regex a) -- Matches the regex in sequence zero or more times
    | RegexMaybe (Regex a) -- Matches the regex zero or one times
    | RegexPlus (Regex a) -- Matches a regex in sequence one or more tmes
    | RegexEmpty -- Matches always
    deriving (Read, Eq)



{- Helper functions for defining the show function on
 - regular expressions to make them an instance of the
 - Show typeclass.
 -
 - This function takes three arguments:
 - the list of literals to match against;
 - a boolean that is only true on first iteration
 - to print the edge case correctly; and
 - a boolean for if the match is inverted -}
show_regex_list :: (Show a) => [a] -> Bool -> Bool -> [Char]
show_regex_list const_list first_iter invert = if first_iter 
    then if null const_list
        then []
        else if invert
            then "[^" ++ show (head const_list) ++ show_regex_list (tail const_list) False invert
            else '[' : show (head const_list) ++ show_regex_list (tail const_list) False invert
    else if null const_list
        then "]"
        else show (head const_list) ++ show_regex_list (tail const_list) False invert

{- Adds parentheses around a regular expression only if it will
 - take more than one character to print. This helps put parentheses
 - only where necessary for pretty printing -}
parens_unless_singleton :: (Show a) => Regex a -> [Char]
parens_unless_singleton re
    | length (show re) > 1 = '(' : (show re) ++ ")"
    | otherwise = show re



{- Define how to print regular expressions. This pretty much
 - follows standard regex syntax -}
instance (Show a) => Show (Regex a) where
    show (RegexAnd []) = []
    show (RegexAnd (re : re_list)) = show re ++ show (RegexAnd re_list)
    show (RegexOr []) = []
    show (RegexOr (re : re_list)) = if null re_list 
        then show re
        else show re ++ "|" ++ show (RegexOr re_list)
    show (RegexConst c) = show c
    show (RegexWord w) = show w
    show (RegexInSet []) = []
    show (RegexInSet const_list) = show_regex_list const_list True False
    show (RegexNotInSet []) = "."
    show (RegexNotInSet const_list) = show_regex_list const_list True True
    show (RegexStar re) = (parens_unless_singleton re) ++ "*" 
    show (RegexMaybe re) = (parens_unless_singleton re) ++ "?"
    show (RegexPlus re) = (parens_unless_singleton re) ++ "+"
    show RegexEmpty = ""
