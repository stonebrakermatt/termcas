{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File defining the keywords for
 - special string literals recognized
 - by this program -}
module IO.Utils.Regex.Keywords
    ( special_funcs
    , discrete_ops
    , constants ) where





{- Trig functions -}
trig_funcs =
    [ "sin"
    , "cos"
    , "tan"
    , "csc"
    , "sec"
    , "cot"
    , "arcsin"
    , "arccos"
    , "arctan"
    , "arccsc"
    , "arcsec"
    , "arccot" ]

{- Hyperbolic trig functions and inverses -}
hyp_trig_funcs =
    [ "sinh"
    , "cosh"
    , "tanh"
    , "csch"
    , "sech"
    , "coth"
    , "arcsinh"
    , "arccosh"
    , "arctanh"
    , "arccsch"
    , "arcsech"
    , "arccoth" ]



{- All special function keywords -}
special_funcs = ["exp", "log", "ln"]
    ++ trig_funcs
    ++ hyp_trig_funcs

{- All binary operator keywords -}
discrete_ops =
    [ "mod"
    , "choose"
    , "permute" ]

{- All keywords for special constants -}
constants =
    [ "pi"
    , "tau"
    , "e"
    , "i"
    , "phi"]
