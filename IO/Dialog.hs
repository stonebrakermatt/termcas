{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File defining help, about, and welcome messages -}
module IO.Dialog where





{- About dialog -}
about_dialog :: [IO ()]
about_dialog =
    [ putStrLn ""
    , putStrLn "TermCAS About Dialog"
    , putStrLn ""
    , putStrLn "TermCAS is free software. Do whatever you want with it."
    , putStrLn "This started when I was learning about LL(k) parsers"
    , putStrLn "in my spare time some years ago. I decided to revive"
    , putStrLn "the project in 2022." ]

{- Help dialogs -}
help_dialog :: Int -> [IO ()]
help_dialog n

    {- Help dialog page 1 -}
    | n == 0 = 
        [ putStrLn ""
        , putStrLn "TermCAS Help Dialog [1/5]"
        , putStrLn ""
        , putStrLn "BASIC USAGE:"
        , putStrLn ""
        , putStrLn "To get started, try defining a variable:"
        , putStrLn "n       => a = 2"
        , putStrLn ""
        , putStrLn "You can use these variables in future expressions:"
        , putStrLn "n       => sin(a)"
        , putStrLn ""
        , putStrLn "To view the next page of the help dialog, type:"
        , putStrLn "n       => \\help 1" ]

    {- Help dialog page 2 -}
    | n == 1 = 
        [ putStrLn ""
        , putStrLn "TermCAS Help Dialog [2/5]"
        , putStrLn ""
        , putStrLn "COMMANDS:"
        , putStrLn ""
        , putStrLn "For general information, type:"
        , putStrLn "n       => \\about"
        , putStrLn ""
        , putStrLn "To view the current variable, function, and"
        , putStrLn "set bindings, type:"
        , putStrLn "n       => \\bindings"
        , putStrLn ""
        , putStrLn "To quit the program, type:"
        , putStrLn "n       => \\exit"
        , putStrLn ""
        , putStrLn "There is also a graphing command, which will be"
        , putStrLn "covered later."
        , putStrLn ""
        , putStrLn "To view the previous page of the help dialog, type:"
        , putStrLn "n       => \\help"
        , putStrLn "        or"
        , putStrLn "n       => \\help 0"
        , putStrLn ""
        , putStrLn "To view the next page of the help dialog, type:"
        , putStrLn "n       => \\help 2" ]
    
    {- Help dialog page 3 -}
    | n == 2 = 
        [ putStrLn ""
        , putStrLn "TermCAS Help Dialog [3/5]"
        , putStrLn ""
        , putStrLn "EXPRESSIONS:"
        , putStrLn ""
        , putStrLn "TermCAS parses your input into chunks called expressions."
        , putStrLn "An expression is something that computes to a value. For"
        , putStrLn "example, imagine we typed:"
        , putStrLn "n       => f(a) + c"
        , putStrLn ""
        , putStrLn "Assuming we have defined the constants a and c and a"
        , putStrLn "function of 1 variable called f, this input will simplify"
        , putStrLn "to a value, and it is therefore an expression."
        , putStrLn ""
        , putStrLn "Simply entering an expression will instruct TermCAS to"
        , putStrLn "evaluate that expression. Alternatively, you can bind an"
        , putStrLn "expression to a variable like so:"
        , putStrLn "n       => b = f(a) + c"
        , putStrLn ""
        , putStrLn "To view the previous page of the help dialog, type:"
        , putStrLn "n       => \\help 1"
        , putStrLn ""
        , putStrLn "To view the next page of the help dialog, type:"
        , putStrLn "n       => \\help 3" ]

    {- Help dialog page 4 -}
    | n == 3 = 
        [ putStrLn ""
        , putStrLn "TermCAS Help Dialog [4/5]"
        , putStrLn ""
        , putStrLn "SETS:"
        , putStrLn ""
        , putStrLn "TermCAS now supports sets. The main purpose of this"
        , putStrLn "feature is for graphing, which is not implemented yet."
        , putStrLn "Like expressions, you can evaluate a set or bind it"
        , putStrLn "to a variable (this time comprised of uppercase letters"
        , putStrLn "only), respectively, like so:"
        , putStrLn "n       => {x in R : x > 2}"
        , putStrLn "        or"
        , putStrLn "n       => S = {x in R : x > 2}"
        , putStrLn ""
        , putStrLn "Sets support interval notation as well, so you could"
        , putStrLn "specify the set {x in R : x > 2 and x <= 3} as simply"
        , putStrLn "(2,3], which is much nicer." 
        , putStrLn ""
        , putStrLn "To view the previous page of the help dialog, type:"
        , putStrLn "n       => \\help 2"
        , putStrLn ""
        , putStrLn "To view the next page of the help dialog, type:"
        , putStrLn "n       => \\help 4" ]

    {- Help dialog page 5 -}
    | n == 4 = 
        [ putStrLn ""
        , putStrLn "TermCAS Help Dialog [5/5]"
        , putStrLn ""
        , putStrLn "GRAPHING:"
        , putStrLn ""
        , putStrLn "This isn't implemented yet."
        , putStrLn ""
        , putStrLn "To view the previous page of the help dialog, type:"
        , putStrLn "n       => \\help 3" ]
