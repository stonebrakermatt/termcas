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
    , putStrLn "in my spare time some years ago." ]

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
        , putStrLn "To view the next page of the help dialog, type:"
        , putStrLn "n       => \\help 1" ]

    {- Help dialog page 2 -}
    | n == 1 = 
        [ putStrLn ""
        , putStrLn "TermCAS Help Dialog [2/5]"
        , putStrLn ""
        , putStrLn "BASIC USAGE:"
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
        , putStrLn "BASIC USAGE:"
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
        , putStrLn "BASIC USAGE:"
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
        , putStrLn "BASIC USAGE:"
        , putStrLn ""
        , putStrLn "To view the previous page of the help dialog, type:"
        , putStrLn "n       => \\help 3" ]
