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
    , putStrLn "in my spare time."
    , putStrLn "" ]

{- Help dialogs -}
help_dialog :: Int -> [IO ()]
help_dialog n

    {- Help dialog page 1 -}
    | n == 0 = 
        [ putStrLn ""
        , putStrLn "TermCAS Help Dialog [1/3]"
        , putStrLn ""
        , putStrLn "BASIC USAGE:"
        , putStrLn ""
        , putStrLn "There are three kinds of things you can type into"
        , putStrLn "the REPL at this time. First, you have built-in"
        , putStrLn "backslash-escaped commands such as \\about,"
        , putStrLn "\\bindings, \\exit, and \\help."
        , putStrLn ""   
        , putStrLn "The second thing you can do is evaluate expressions:"
        , putStrLn "0       => sin(2)"
        , putStrLn ""
        , putStrLn "Finally, you can define variables and functions:"
        , putStrLn "1       => a = 2 + pi"
        , putStrLn "2       => f(x) = e^x"
        , putStrLn ""
        , putStrLn "Now, you can use the values you've already defined"
        , putStrLn "in your later inputs:"
        , putStrLn "3       => f(a)"
        , putStrLn ""
        , putStrLn "To view the next page of the help dialog, type:"
        , putStrLn "n       => \\help 1"
        , putStrLn "" ]

    {- Help dialog page 2 -}
    | n == 1 =
        [ putStrLn ""
        , putStrLn "TermCAS Help Dialog [2/3]"
        , putStrLn ""
        , putStrLn "BASIC USAGE:"
        , putStrLn ""
        , putStrLn "There are three kinds of things you can type into"
        , putStrLn "the REPL at this time. First, you have built-in"
        , putStrLn "backslash-escaped commands such as \\about,"
        , putStrLn "\\bindings, \\exit, and \\help."
        , putStrLn ""   
        , putStrLn "The second thing you can do is evaluate expressions:"
        , putStrLn "0       => sin(2)"
        , putStrLn ""
        , putStrLn "Finally, you can define variables and functions:"
        , putStrLn "1       => a = 2 + pi"
        , putStrLn "2       => f(x) = e^x"
        , putStrLn ""
        , putStrLn "Now, you can use the values you've already defined"
        , putStrLn "in your later inputs:"
        , putStrLn "3       => f(a)"
        , putStrLn ""
        , putStrLn "To view the next page of the help dialog, type:"
        , putStrLn "n       => \\help 2"
        , putStrLn "" ]
    
    {- Help dialog page 3 -}
    | n == 2 = 
        [ putStrLn ""
        , putStrLn "TermCAS Help Dialog [3/3]"
        , putStrLn ""
        , putStrLn "BASIC USAGE:"
        , putStrLn ""
        , putStrLn "There are three kinds of things you can type into"
        , putStrLn "the REPL at this time. First, you have built-in"
        , putStrLn "backslash-escaped commands such as \\about,"
        , putStrLn "\\bindings, \\exit, and \\help."
        , putStrLn ""   
        , putStrLn "The second thing you can do is evaluate expressions:"
        , putStrLn "0       => sin(2)"
        , putStrLn ""
        , putStrLn "Finally, you can define variables and functions:"
        , putStrLn "1       => a = 2 + pi"
        , putStrLn "2       => f(x) = e^x"
        , putStrLn ""
        , putStrLn "Now, you can use the values you've already defined"
        , putStrLn "in your later inputs:"
        , putStrLn "3       => f(a)"
        , putStrLn ""
        , putStrLn "This is the last page of the help dialog."
        , putStrLn "" ]

{- Welcome dialog -}
welcome_dialog :: [IO ()]
welcome_dialog =
    [ putStrLn ""
    , putStrLn "TermCAS v.0.1.0 - try \"\\about\" or \"\\help\""
    , putStrLn "\"\\exit\" to quit"
    , putStrLn "" ]
