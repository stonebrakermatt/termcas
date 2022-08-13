{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File defining a regular expression data type
 - as well as functions for how to display and
 - read data of this type -}
module IO.Utils.Regex.Grammar where
import qualified IO.Utils.Regex.Keywords as Keywords
import qualified IO.Utils.Regex.Type as RE





{- Regular expression for builtin commands -}
regex_command :: RE.Regex
regex_command = RE.RegexAnd
    [ RE.RegexConst '\\'
    , RE.RegexPlus (RE.RegexInSet ['a'..'z']) ]

{- Regular expression for keywords -}
regex_keyword :: RE.Regex
regex_keyword = RE.RegexOr (map RE.RegexWord Keywords.keywords)

{- Regular expression to match digits 1-9 -}
regex_nonzero_digit :: RE.Regex
regex_nonzero_digit = RE.RegexInSet ['1'..'9']

{- Regular expression to match zero -}
regex_zero :: RE.Regex
regex_zero = RE.RegexConst '0'

{- Regular expression to match digits 0-9 -}
regex_digit :: RE.Regex
regex_digit = RE.RegexInSet ['0'..'9']

{- Regular expressions to match the integral and fractional
 - parts of a number with a decimal point -}
regex_number_integer_part :: RE.Regex
regex_number_integer_part = RE.RegexOr
    [ regex_zero
    , RE.RegexAnd
        [ regex_nonzero_digit
        , RE.RegexStar (regex_digit) ]]
regex_number_fractional_part :: RE.Regex
regex_number_fractional_part = RE.RegexAnd
    [ RE.RegexConst '.'
    , RE.RegexPlus (regex_digit) ]

{- Handles the exponential part of a number entered
 - in scientific notation -}
regex_number_exponential_part :: RE.Regex
regex_number_exponential_part = RE.RegexAnd
    [ RE.RegexOr
        [ RE.RegexConst 'E'
        , RE.RegexConst 'e' ]
    , RE.RegexMaybe (RE.RegexConst '-')
    , regex_number_integer_part ]

{- Regular expression for matching positive numbers
 - (negative numbers obtained through negation, which is an
 - operation); supports scientific notation, integers, and floats -}
regex_number :: RE.Regex
regex_number = RE.RegexAnd
    [ RE.RegexOr
        [ RE.RegexAnd
            [ regex_number_integer_part
            , RE.RegexMaybe regex_number_fractional_part ]
        , regex_number_fractional_part ]
    , RE.RegexMaybe regex_number_exponential_part ]

{- Regular expression for matching positive integers only -}
regex_number_integer :: RE.Regex
regex_number_integer = RE.RegexAnd
    [ regex_number_integer_part
    , RE.RegexMaybe regex_number_exponential_part ]

{- Regular expression for boolean values -}
regex_boolean :: RE.Regex
regex_boolean = RE.RegexOr
    [ RE.RegexWord "True" 
    , RE.RegexWord "False" ]

{- Regular expression for matching identifiers for
 - variables, functions, etc. -}
regex_identifier :: RE.Regex
regex_identifier = RE.RegexPlus (RE.RegexInSet ['a'..'z'])

{- Regular expression for matching identifiers for sets,
 - which are denoted by capital letters -}
regex_sidentifier :: RE.Regex
regex_sidentifier = RE.RegexPlus (RE.RegexInSet ['A'..'Z'])

{- Regular expression for assigning -}
regex_assign :: RE.Regex
regex_assign = RE.RegexConst '='

{- Regular expression for matching any delimiters
 - ("=", "(", ")", "{". "}", and "," -- so far!) -}
regex_delimiter :: RE.Regex
regex_delimiter = RE.RegexInSet "(){}[]:,"

{- Regular expressions for prefix unary minus and
 - postfix factorial and prime notation derivatives -}
regex_unary_operator :: RE.Regex
regex_unary_operator = RE.RegexOr
    [ RE.RegexInSet "-!"
    , RE.RegexWord "not" ]

{- Regular expression for parsing binary expressions -}
regex_binary_operator :: RE.Regex
regex_binary_operator = RE.RegexOr
    (RE.RegexInSet "+-*/^" : map RE.RegexWord Keywords.discrete_ops ++ 
        [ RE.RegexWord "=="
        , RE.RegexWord ">="
        , RE.RegexWord "<="
        , RE.RegexWord "and"
        , RE.RegexWord "or"
        , RE.RegexWord "xor"
        , RE.RegexInSet "<>" ])

{- Regular expression for operators -}
regex_operator :: RE.Regex
regex_operator = RE.RegexOr
    [ regex_unary_operator
    , regex_binary_operator ]

{- Regular expression for whitespace -}
regex_space :: RE.Regex
regex_space = RE.RegexInSet " \t"

{- Regular expression for invalid input -}
regex_badinput :: RE.Regex
regex_badinput = RE.RegexNotInSet []



{- All regexes for this grammar -}
language_regexes =
    [ regex_command
    , regex_keyword
    , regex_operator
    , regex_assign
    , regex_delimiter
    , regex_number
    , regex_boolean
    , regex_identifier
    , regex_sidentifier
    , regex_space
    , regex_badinput ]
