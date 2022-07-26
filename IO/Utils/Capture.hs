{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 - 
 - File for capturing regular expressions 
 - off the head of a list -}
module IO.Utils.Capture ( capture ) where
import qualified IO.Utils.Regex.Type as R

{- Capture function to handle matching regular expressions
 - against user input. If the function does not return nothing, 
 - the tuple consists first of the matching part of the input and
 - second of the remaining input to be lexed -}
capture :: (Eq a) => [a] -> R.Regex a -> Maybe ([a], [a])

{- Matches a list of regexes in sequence;
 - since it works in sequence, the RegexAnd of
 - an empty list is a base case that returns the same
 - result as capturing RegexEmpty -}
user_input `capture` (R.RegexAnd []) = Just([], user_input)
user_input `capture` (R.RegexAnd (re : re_list)) = case user_input `capture` re of
    Just (match, input_tail) -> case input_tail `capture` (R.RegexAnd re_list) of
        Just (match', input_remainder) -> Just (match ++ match', input_remainder)
        ok -> ok
    ok -> ok

{- Matches any of the regexes in the list;
 - unlike RegexAnd, the RegexOr of zero regular
 - expressions is Nothing since the base case 
 - means no matches were found -}
user_input `capture` (R.RegexOr []) = Nothing
user_input `capture` (R.RegexOr (re : re_list)) = case user_input `capture` re of 
    Nothing -> user_input `capture` (R.RegexOr re_list)
    ok -> ok

{- Matches a single literal value only -}
[] `capture` (R.RegexConst c) = Nothing
(letter : user_input) `capture` (R.RegexConst c)
    | letter == c = Just ([c], user_input)
    | otherwise = Nothing

{- Matches a string of literals in sequence
 - user_input `capture` (R.RegexWord word) 
 - is equivalent to writing R.RegexAnd (Data.List.map R.RegexConst word) -}
(letter : user_input) `capture` (R.RegexWord (c : word))
    | letter /= c = Nothing
    | otherwise = case user_input `capture` (R.RegexWord word) of
        Just (match, input_tail) -> Just (c : match, input_tail) 
        ok -> ok
user_input `capture` (R.RegexWord []) = Just ([], user_input)

{- Matches any of a list of literal values or
 - Nothing if no match is found -}
user_input `capture` (R.RegexInSet []) = Nothing
user_input `capture` (R.RegexInSet (c : const_list)) = case user_input `capture` (R.RegexConst c) of
    Nothing -> user_input `capture` (R.RegexInSet const_list)
    ok -> ok

{- Inverse of the above; matches any literal value not
 - specified in the given list -}
(letter : user_input) `capture` (R.RegexNotInSet []) = Just ([letter], user_input)
(letter : user_input) `capture` (R.RegexNotInSet (c : const_list))
    | letter == c = Nothing
    | otherwise = (letter : user_input) `capture` (R.RegexNotInSet const_list) 

{- Matches the regex in sequence zero or more times;
 - guaranteed to return something other than Nothing -}
user_input `capture` (R.RegexStar re) = case user_input `capture` re of
    Nothing -> Just ([], user_input)
    Just (match, input_tail) -> 
        let Just (match', input_tail') = input_tail `capture` (R.RegexStar re)
        in Just (match ++ match', input_tail')
        
{- Matches the regex zero or one times;
 - guaranteed to return something other than Nothing -}
user_input `capture` (R.RegexMaybe re) = case user_input `capture` re of
    Nothing -> Just ([], user_input)
    ok -> ok

{- Matches the regex in sequence one or more times;
 - unlike RegexStar, this can fail -}
user_input `capture` (R.RegexPlus re) = user_input `capture` (R.RegexAnd [re, R.RegexStar re])

{- Always matches -}
user_input `capture` R.RegexEmpty = Just ([], user_input)

{- Fallback for excluded null list patterns -}
[] `capture` re = Nothing 
