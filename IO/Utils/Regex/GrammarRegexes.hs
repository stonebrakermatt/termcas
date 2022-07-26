{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File defining a regular expression data type
 - as well as functions for how to display and
 - read data of this type -}
module IO.Utils.Regex.GrammarRegexes where
import qualified IO.Utils.Regex.Keywords as K
import qualified IO.Utils.Regex.Type as R





{- Regular expression to match digits 1-9 -}
regex_nonzero_digit :: R.Regex Char
regex_nonzero_digit = R.RegexInSet ['1'..'9']

{- Regular expression to match zero -}
regex_zero :: R.Regex Char
regex_zero = R.RegexConst '0'

{- Regular expression to match digits 0-9 -}
regex_digit :: R.Regex Char
regex_digit = R.RegexOr
    [ regex_nonzero_digit
    , regex_zero ]

{- Regular expressions to match the integral and fractional
 - parts of a number with a decimal point -}
regex_number_integer_part :: R.Regex Char
regex_number_integer_part = R.RegexOr
    [ regex_zero
    , R.RegexAnd
        [ regex_nonzero_digit
        , R.RegexStar (regex_digit) ]]
regex_number_fractional_part :: R.Regex Char
regex_number_fractional_part = R.RegexAnd
    [ R.RegexConst '.'
    , R.RegexPlus (regex_digit) ]

{- Handles the exponential part of a number entered
 - in scientific notation -}
regex_number_exponential_part :: R.Regex Char
regex_number_exponential_part = R.RegexAnd
    [ R.RegexOr
        [ R.RegexConst 'E'
        , R.RegexConst 'e' ]
    , regex_number_integer_part ]

{- Regular expression for matching integers -}
regex_int :: R.Regex Char
regex_int = R.RegexAnd
    [ regex_number_integer_part
    , R.RegexMaybe regex_number_exponential_part ]

{- Regular expression for matching positive numbers
 - (negative numbers obtained through negation, which is an
 - operation); supports scientific notation, integers, and floats -}
regex_number :: R.Regex Char
regex_number = R.RegexOr
    ( R.RegexAnd
        [ R.RegexOr
            [ R.RegexAnd
                [ regex_number_integer_part
                , R.RegexMaybe regex_number_fractional_part ]
            , regex_number_fractional_part ]
        , R.RegexMaybe regex_number_exponential_part ]
    : map R.RegexWord K.constants )

{- Regular expression for matching identifiers for
 - variables, functions, sets, etc. -}
regex_identifier :: R.Regex Char
regex_identifier = R.RegexPlus (R.RegexOr
    [ R.RegexInSet ['a'..'z']
    , R.RegexInSet ['A'..'Z'] ])

{- Regular expression for matching any delimiters
 - ("=", "(", ")", "{". "}", and "," -- so far!) -}
regex_delimiter :: R.Regex Char
regex_delimiter = R.RegexInSet "=(),"

{- Regular expressions for prefix unary minus and
 - postfix factorial and prime notation derivatives -}
regex_unary_operator :: R.Regex Char
regex_unary_operator = R.RegexConst '-'

{- Regular expression for parsing binary expressions -}
regex_binary_operator :: R.Regex Char
regex_binary_operator = R.RegexOr
    (R.RegexInSet "+-*/^" : map R.RegexWord K.discrete_ops)

{- Regular expression for invalid input -}
regex_badinput :: R.Regex Char
regex_badinput = R.RegexNotInSet []

{- Regular expression for operators -}
regex_operator :: R.Regex Char
regex_operator = R.RegexOr
    [ regex_unary_operator
    , regex_binary_operator ]

{- Regular expression for whitespace -}
regex_space :: R.Regex Char
regex_space = R.RegexInSet " \t"



{- All regexes for this grammar -}
language_regexes =
    [ regex_delimiter
    , regex_operator
    , regex_number
    , regex_identifier
    , regex_space
    , regex_badinput ]
