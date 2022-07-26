{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File defining help, about, and welcome messages -}
module IO.Dialog where





{- About, help, and welcome messages -}
about_dialog :: [IO ()]
about_dialog =
    [ putStrLn ""
    , putStrLn "TermCAS is free software. Do whatever you want with it."
    , putStrLn "" ]
help_dialog :: [IO ()]
help_dialog =
    [ putStrLn ""
    , putStrLn "Basic usage:"
    , putStrLn ""
    , putStrLn "1) \"<var>\""
    , putStrLn "In this case, the expression will be evaluated."
    , putStrLn ""
    , putStrLn "2) \"<var>=<expr>\""
    , putStrLn "In this case, a variable bound to the string \"var\""
    , putStrLn "will be created (or overwritten, if one existed)."
    , putStrLn "These variables can then be used in evaluating expressions."
    , putStrLn "" ]
welcome_dialog :: [IO ()]
welcome_dialog =
    [ putStrLn ""
    , putStrLn "TermCAS v.0.1.0 - try \"\\about\" or \"\\help\""
    , putStrLn "\"\\exit\" to quit"
    , putStrLn "" ]
