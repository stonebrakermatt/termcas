{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 - 
 - File for capturing regular expressions 
 - off the head of a list -}
module IO.Utils.Capture ( capture ) where
import qualified IO.Utils.Regex.Type as RE





{- Capture function to handle matching regular expressions
 - against user input. If the function does not return nothing, 
 - the tuple consists first of the matching part of the input and
 - second of the remaining input to be lexed -}
capture :: [Char] -> RE.Regex -> Maybe ([Char], [Char])

{- Matches a list of regexes in sequence;
 - since it works in sequence, the RegexAnd of
 - an empty list is a base case that returns the same
 - result as capturing RegexEmpty -}
user_input `capture` (RE.RegexAnd []) = Just([], user_input)
user_input `capture` (RE.RegexAnd (re : re_list)) = case user_input `capture` re of
    Just (match, input_tail) -> case input_tail `capture` (RE.RegexAnd re_list) of
        Just (match', input_remainder) -> Just (match ++ match', input_remainder)
        ok -> ok
    ok -> ok

{- Matches any of the regexes in the list;
 - unlike RegexAnd, the RegexOr of zero regular
 - expressions is Nothing since the base case 
 - means no matches were found -}
user_input `capture` (RE.RegexOr []) = Nothing
user_input `capture` (RE.RegexOr (re : re_list)) = case user_input `capture` re of 
    Nothing -> user_input `capture` (RE.RegexOr re_list)
    ok -> ok

{- Matches a single literal value only -}
[] `capture` (RE.RegexConst c) = Nothing
(letter : user_input) `capture` (RE.RegexConst c)
    | letter == c = Just ([c], user_input)
    | otherwise = Nothing

{- Matches a string of literals in sequence
 - user_input `capture` (R.RegexWord word) 
 - is equivalent to writing R.RegexAnd (Data.List.map R.RegexConst word) -}
(letter : user_input) `capture` (RE.RegexWord (c : word))
    | letter /= c = Nothing
    | otherwise = case user_input `capture` (RE.RegexWord word) of
        Just (match, input_tail) -> Just (c : match, input_tail) 
        ok -> ok
user_input `capture` (RE.RegexWord []) = Just ([], user_input)

{- Matches any of a list of literal values or
 - Nothing if no match is found -}
user_input `capture` (RE.RegexInSet []) = Nothing
user_input `capture` (RE.RegexInSet (c : const_list)) = case user_input `capture` (RE.RegexConst c) of
    Nothing -> user_input `capture` (RE.RegexInSet const_list)
    ok -> ok

{- Inverse of the above; matches any literal value not
 - specified in the given list -}
(letter : user_input) `capture` (RE.RegexNotInSet []) = Just ([letter], user_input)
(letter : user_input) `capture` (RE.RegexNotInSet (c : const_list))
    | letter == c = Nothing
    | otherwise = (letter : user_input) `capture` (RE.RegexNotInSet const_list) 

{- Matches the regex in sequence zero or more times;
 - guaranteed to return something other than Nothing -}
user_input `capture` (RE.RegexStar re) = case user_input `capture` re of
    Nothing -> Just ([], user_input)
    Just (match, input_tail) -> 
        let Just (match', input_tail') = input_tail `capture` (RE.RegexStar re)
        in Just (match ++ match', input_tail')
        
{- Matches the regex zero or one times;
 - guaranteed to return something other than Nothing -}
user_input `capture` (RE.RegexMaybe re) = case user_input `capture` re of
    Nothing -> Just ([], user_input)
    ok -> ok

{- Matches the regex in sequence one or more times;
 - unlike RegexStar, this can fail -}
user_input `capture` (RE.RegexPlus re) = user_input `capture` (RE.RegexAnd [re, RE.RegexStar re])

{- Always matches -}
user_input `capture` RE.RegexEmpty = Just ([], user_input)

{- Fallback for excluded null list patterns -}
[] `capture` re = Nothing 
