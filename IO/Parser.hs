{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File for parsing input -}
module IO.Parser where
import qualified ExpData.Expression.Type as Exp
import qualified ExpData.Expression.Utils as ExpUtils
import qualified IO.Utils.Lexer as Lexer
import qualified IO.Utils.Token as Token





{- Error type for helpful feedback -}
data ParseError
    = FailedToBuildExpression
    | MismatchedBrackets
    | UnexpectedEndOfInput
    | UnexpectedInput [Char]
    | IntervalFormatError
    | SetFormatError
    | TooManyArguments -- for commands when implemented
    | UnknownArgument [Char]
    deriving (Read)

{- Display parse errors nicely -}
instance Show ParseError where
    show FailedToBuildExpression = "Err: Failed to build expression."
    show MismatchedBrackets = "Err: Mismatched brackets."
    show UnexpectedEndOfInput = "Err: Unexpected end of input."
    show (UnexpectedInput str) = "Err: Unexpected input: " ++ str ++ "."
    show IntervalFormatError = "Err: Interval formatting."
    show SetFormatError = "Err: Set formatting."
    show TooManyArguments = "Err: Too many arguments."
    show (UnknownArgument str) = "Err: Unknown argument: " ++ str ++ "."

{- Parser type for simplpifying type signatures -}
type ParseResult t = Either (t, [Token.InputToken]) ParseError
type Parser t = [Token.InputToken] -> ParseResult t



{- Bind utility for chaining our
 - mutually recursive parsing functions -}
pbind :: Either a b -> (a -> Either c b) -> Either c b
pbind (Right e) f = Right e
pbind (Left val) f = f val

{- Opposite of bind, basically -}
pfails :: Either a b -> (() -> Either a b) -> Either a b
pfails (Right e) f = f()
pfails ok _ = ok



{- Utilities for the simplifying the parser as
 - much as possible. Still complicated, but it helps -}

{- Gets the next token.
 - Plays nice with pbind and pfails -}
next_token :: Parser Token.InputToken
next_token (t : input1) = Left (t, input1)
next_token _ = Right UnexpectedEndOfInput

{- Parses an operator with the appropriate precedence.
 - Used in parse_aoptail to maintain the order of operaions -}
parse_op_prec :: Int -> Parser Exp.Op
parse_op_prec n [] = Right UnexpectedEndOfInput
parse_op_prec n (t : input) = case t of
    Token.OpToken o ->
        if Exp.operator_precedence (Exp.get_op o) == n
        then Left (Exp.get_op o, input)
        else Right (UnexpectedInput (show t))
    _ -> Right (UnexpectedInput (show t))

{- Helper function for parse_op. Takes the results of 
 - parse_optail and builds out the expression tree -}
build_tail :: ([Exp.Expression], [Exp.Op]) -> Either Exp.Expression ParseError
build_tail ([], _) = Right FailedToBuildExpression
build_tail ([e], _) = Left e
build_tail (_, []) = Right FailedToBuildExpression
build_tail (e1 : e2 : exps, o : ops) =
    build_tail (Exp.Binary o e1 e2 : exps, ops)

{- Extracts a parenthetical for nested parsing.
 - Used for function call arguments as well as
 - simple parenthetical expressions -}
extract_parenthetical :: Parser [Token.InputToken]
extract_parenthetical input =
    let extract_parenthetical' depth [] revtokens = Right MismatchedBrackets
        extract_parenthetical' depth (t : tokens) revtokens = case t of
            Token.DelimiterToken "(" ->
                extract_parenthetical' (depth + 1) tokens (t : revtokens)
            Token.DelimiterToken ")" ->
                if depth == 1
                then Left (reverse revtokens, tokens)
                else extract_parenthetical' (depth - 1) tokens (t : revtokens)
            _ -> extract_parenthetical' depth tokens (t : revtokens)
    in extract_parenthetical' 1 input []

{- Separates function call arguments into a list
 - of expressions. It is built to be mindful of
 - nesting depth to avoid splitting the arguments
 - if one of the function call arguments itself
 - contains a multivariable function call -}
separate_args :: Parser [Exp.Expression]
separate_args input =
    let separate_args' depth (t : tokens) revtokens revargs = case t of
            Token.DelimiterToken "(" ->
                separate_args' (depth + 1) tokens (t : revtokens) revargs
            Token.DelimiterToken ")" ->
                if depth == 0
                then Right MismatchedBrackets
                else separate_args' (depth - 1) tokens (t : revtokens) revargs
            Token.DelimiterToken "," ->
                if depth == 0
                then case parse_expr (reverse revtokens) of
                    Left (new_arg, _) -> separate_args' depth tokens [] (new_arg : revargs)
                    Right e -> Right e
                else separate_args' depth tokens (t : revtokens) revargs
            _ -> separate_args' depth tokens (t : revtokens) revargs
        separate_args' depth [] revtokens revargs = if length revtokens > 0
            then case parse_expr (reverse revtokens) of
                Left (new_arg, _) -> Left (reverse (new_arg : revargs), [])
                Right e -> Right e
            else Left (reverse revargs, [])
    in separate_args' 0 input [] []



{- The main parser functionality for expressions starts below -}

{- Rule: Expr -> AAtail -}
parse_expr :: Parser Exp.Expression
parse_expr input = case parse_operand 0 input of
    Left (expr, tokens) -> Left (expr, tokens)
    Right err -> Right err

{- Rules:
 - Expr -> not AAtail | AAtail
 - A -> BBtail
 - B -> -CCtail | CCtail
 - C -> DDtail 
 - D -> EEtail 
 - E -> FFtail -}
parse_operand :: Int -> Parser Exp.Expression
parse_operand prec input = if prec < 6
    then if prec == 4
        then
            next_token input `pbind` (\(t, input1) -> 
            case t of 
                Token.OpToken "-" -> 
                    parse_operand (prec + 1) input1 `pbind` (\(e, input2) ->
                    parse_optail (prec + 1) (Exp.Negate e) input2 `pbind` (\(pairs, input3) ->
                    build_tail pairs `pbind` (\exp -> Left (exp,  input3))))
                _ ->
                    parse_operand (prec + 1) input `pbind` (\(e, input2) ->
                    parse_optail (prec + 1) e input2 `pbind` (\(pairs, input3) ->
                    build_tail pairs `pbind` (\exp -> Left (exp,  input3)))))
        else if prec == 1
            then 
                next_token input `pbind` (\(t, input1) ->
                    case t of
                        Token.OpToken "not" ->
                            parse_operand (prec + 1) input1 `pbind` (\(e, input2) ->
                            parse_optail (prec + 1) (Exp.Not e) input2 `pbind` (\(pairs, input3) ->
                            build_tail pairs `pbind` (\exp -> Left (exp,  input3))))
                        _ -> 
                            parse_operand (prec + 1) input `pbind` (\(e, input2) ->
                            parse_optail (prec + 1) e input2 `pbind` (\(pairs, input3) ->
                            build_tail pairs `pbind` (\exp -> Left (exp, input3)))))
            else 
                parse_operand (prec + 1) input `pbind` (\(e, input1) ->
                parse_optail (prec + 1) e input1 `pbind` (\(pairs, input2) ->
                build_tail pairs `pbind` (\exp -> Left (exp, input2))))
    else
        parse_f input

{- Rules:
 - Atail -> XAAtail | Empty
 - Btail -> YBBtail | Empty
 - Ctail -> ZCCtail | Empty
 - Dtail -> \+DDtail | \-DDtail | Empty 
 - Etail -> \*EEtail | \/EEtail | Empty
 - Ftail -> \^FFtail | Empty 
 -
 - X -> and | or | xor 
 - Y -> == | != | > | >= | < | <= 
 - Z -> mod | choose | permute -}
parse_optail :: Int -> Exp.Expression -> Parser ([Exp.Expression], [Exp.Op])
parse_optail prec exp input =
    let parse_optail' [] (exps, ops) = Left ((reverse exps, reverse ops), [])
        parse_optail' input (exps, ops) =
            parse_op_prec prec input `pbind` (\(o, input1) ->
            parse_operand prec input1 `pbind` (\(e, input2) ->
            parse_optail' input2 (e : exps, o : ops))) `pfails` (\() ->
            Left ((reverse exps, reverse ops), input))
    in parse_optail' input ([exp], [])

{- Rule: G -> IdOrCall | Num | Bool | Parens -}
parse_term :: Parser Exp.Expression
parse_term input =
    parse_id_or_call input `pfails` (\() ->
    parse_num input `pfails` (\() ->
    parse_bool input `pfails` (\() ->
    parse_parens input)))

{- Rule: F -> G!* -}
parse_factorial :: Exp.Expression -> Parser Exp.Expression
parse_factorial exp input = 
    if input == []
        then Left (exp, [])
        else next_token input `pbind` (\(t, input1) ->
            case t of
                Token.OpToken "!" -> parse_factorial (Exp.Factorial exp) input1
                _ -> Left (exp, input)) 

{- Rule: F -> IdOrCall!* | Num!* | Bool!* | Parens!* -}
parse_f :: Parser Exp.Expression
parse_f input = 
    parse_term input `pbind` (\(exp, input1) ->
    parse_factorial exp input1)



{- Helper functions for parse_id_or_call -}
parse_id :: Parser Exp.Expression
parse_id input =
    next_token input `pbind` (\(t, input1) ->
    case t of
        Token.IdToken id -> Left (Exp.Id id, input1)
        _ -> Right (UnexpectedInput (show t)))
parse_call :: Parser [Exp.Expression]
parse_call input =
    next_token input `pbind` (\(t, input1) ->
    case t of
        Token.DelimiterToken "(" ->
            extract_parenthetical input1 `pbind` (\(parens, input2) ->
            separate_args parens `pbind` (\(args, _) ->
            Left (args, input2)))
        _ -> Right (UnexpectedInput (show t)))

{- Rule: IdOrCall -> Id (Args | Empty) -}
parse_id_or_call :: Parser Exp.Expression
parse_id_or_call input =
    parse_id input `pbind` (\(eid, input1) ->
        case eid of
            Exp.Id id -> parse_call input1 `pbind` (\(args, input2) ->
                Left (Exp.FCall id args, input2)) `pfails` (\() -> Left (eid, input1))
            _ -> Right (UnexpectedInput (show eid)))

{- Parses a number or boolean or parenthetical if
 - parse_id_or_call fails -}
parse_num :: Parser Exp.Expression
parse_num input =
    next_token input `pbind` (\(t, input1) ->
    case t of
        Token.NumLiteralToken num -> Left (Exp.Num num, input1)
        _ -> Right (UnexpectedInput (show t)))
parse_bool :: Parser Exp.Expression
parse_bool input = next_token input `pbind` (\(t, input1) ->
    case t of 
        Token.BooleanToken "True" -> Left (Exp.Boolean True, input1)
        Token.BooleanToken "False" -> Left (Exp.Boolean False, input1)
        _ -> Right (UnexpectedInput (show t)))
parse_parens :: Parser Exp.Expression
parse_parens input =
    next_token input `pbind` (\(t, input1) ->
    case t of
        Token.DelimiterToken "(" ->
            extract_parenthetical input1 `pbind` (\(parens, input2) ->
            parse_expr parens `pbind` (\(exp, _) ->
            Left (Exp.Parenthetical exp, input2)))
        _ -> Right (UnexpectedInput (show t)))



{- Functionality for parsing sets starts below -}

{- Helper function to build a set -}
build_stail :: ([Exp.Set], [Exp.Op]) -> Either Exp.Set ParseError
build_stail ([], _) = Right FailedToBuildExpression
build_stail ([s], _) = Left s
build_stail (_, []) = Right FailedToBuildExpression
build_stail (s1 : s2 : sets, o : ops) =
    case o of 
        Exp.Times -> build_stail (Exp.STimes s1 s2 : sets, ops)
        _ -> Right (UnexpectedInput (show o)) 

{- Helper functions to parse intervals -}
extract_interval :: Parser ([Token.InputToken], Bool) 
extract_interval input = 
    let extract_interval' [] revtokens = Right UnexpectedEndOfInput
        extract_interval' (t : tokens) revtokens = case t of 
            Token.DelimiterToken ")" -> Left ((reverse revtokens, False), tokens)
            Token.DelimiterToken "]" -> Left ((reverse revtokens, True), tokens)
            _ -> extract_interval' tokens (t : revtokens)
    in extract_interval' input []
split_interval :: Parser [Token.InputToken]
split_interval input = 
    let split_interval' [] revtokens = Right IntervalFormatError
        split_interval' (t : tokens) revtokens = case t of
            Token.DelimiterToken "," -> Left (reverse revtokens, tokens)
            _ -> split_interval' tokens (t : revtokens)
    in split_interval' input []

{- Helper functions for parsing set comprehensions -}

{- Extract set comprehension and split at the colon -}
extract_set :: Parser [Token.InputToken]
extract_set input =
    let extract_set' depth [] revtokens = Right MismatchedBrackets
        extract_set' depth (t : tokens) revtokens = case t of
            Token.DelimiterToken "{" ->
                extract_set' (depth + 1) tokens (t : revtokens)
            Token.DelimiterToken "}" ->
                if depth == 1
                then Left (reverse revtokens, tokens)
                else extract_set' (depth - 1) tokens (t : revtokens)
            _ -> extract_set' depth tokens (t : revtokens)
    in extract_set' 1 input []
split_set :: Parser [Token.InputToken]
split_set input =
    let split_set' [] revtokens = Right SetFormatError
        split_set' (t : tokens) revtokens = case t of 
            Token.DelimiterToken ":" -> Left (reverse revtokens, tokens)
            _ -> split_set' tokens (t : revtokens)
    in split_set' input []

{- Extract set params and base set -}
extract_pb :: Parser ([Token.InputToken], Exp.Set)
extract_pb [] = Right SetFormatError
extract_pb input
    | length input == 1 = Left ((input, Exp.SetId "R"), [])
    | otherwise = next_token input `pbind` (\(t, input1) ->
        case t of
            Token.DelimiterToken "(" -> 
                case extract_parenthetical input1 of 
                    Left (parens, input2) -> 
                        next_token input2 `pbind` (\(t, input3) -> case t of 
                            Token.KeywordToken "in" -> case parse_set input3 of
                                Left (base, input4) -> Left ((parens, base), input4)
                                Right err -> Right err
                            _ -> Right (UnexpectedInput (show t)))
                    Right err -> Right err
            Token.IdToken id -> next_token input1 `pbind` (\(t', input2) ->
                case t' of
                    Token.KeywordToken "in" -> case parse_set input2 of 
                        Left (base, input3) -> Left (([Token.IdToken id], base), input3)
                        Right err -> Right err
                    _ -> Right (UnexpectedInput (show t)))
            _ -> Right SetFormatError)

{- Split at commas -}
split_comma :: [Token.InputToken] -> [[Token.InputToken]]
split_comma input = 
    let split_comma' [] revlist [] = reverse revlist
        split_comma' [] revlist revacc = reverse (reverse revacc : revlist)
        split_comma' (t : tokens) revlist revacc  = case t of
            Token.DelimiterToken "," -> split_comma' tokens (reverse revacc : revlist) []
            _ -> split_comma' tokens revlist (t : revacc)
    in split_comma' input [] []

{- Maps parse_expr over a list of lists of tokens -}
map_parse_expr :: [[Token.InputToken]] -> ParseResult [Exp.Expression]
map_parse_expr input = 
    let map_parse_expr' [] revexps = Left (reverse revexps, [])
        map_parse_expr' (t : tlist) revexps = case parse_expr t of
            Left (exp, []) -> map_parse_expr' tlist (exp : revexps)
            Left _ -> Right SetFormatError
            Right err -> Right err
    in map_parse_expr' input []

map_substitute :: [Char] -> Exp.Expression -> [Exp.Expression] -> [Exp.Expression]
map_substitute x e exprs = map (\e' -> ExpUtils.substitute x e e') exprs

reindex :: [Exp.Expression] -> [Exp.Expression] -> ParseResult ([Exp.Expression], [Exp.Expression])
reindex params conds =
    let reindex' n [] revparams conds = Left ((reverse revparams, conds), [])
        reindex' n (p : params) revparams conds = case p of 
            Exp.Id id -> reindex' (n + 1) params ((Exp.Id ("_x" ++ show n)) : revparams) (map_substitute id (Exp.Id ("_x" ++ show n)) conds)
            _ -> Right SetFormatError
    in reindex' 0 params [] conds
         



{- Main set parsing functionality starts below -}

parse_set :: Parser Exp.Set
parse_set input = parse_sf input

parse_sterm :: Parser Exp.Set
parse_sterm input = parse_sliteral input `pfails` (\() ->
    parse_interval input `pfails` (\() ->
    parse_sid input))

parse_sf :: Parser Exp.Set
parse_sf input = parse_sterm input `pbind` (\(sterm, input1) ->
    case input1 of
        [] -> Left (sterm, [])
        (t : input2) -> case t of 
            Token.OpToken "*" -> parse_sftail sterm input1 `pbind` (\(pairs, input2) ->
                build_stail pairs `pbind` (\set -> Left (set, input2)))
            _ -> Right (UnexpectedInput (show t)))

parse_sftail :: Exp.Set -> Parser ([Exp.Set], [Exp.Op])
parse_sftail set input = 
    let parse_sftail' [] (sets, ops) = Left ((reverse sets, reverse ops), [])
        parse_sftail' input (sets, ops) =
            parse_op_prec 5 input `pbind` (\(o, input1) ->
            parse_sf input1 `pbind` (\(s, input2) ->
            parse_sftail' input2 (s : sets, o : ops))) `pfails` (\() ->
            Left ((reverse sets, reverse ops), input))
    in parse_sftail' input ([set], [])

{- Parses a set id -}
parse_sid :: Parser Exp.Set
parse_sid input = 
    next_token input `pbind` (\(t, input1) ->
        case t of 
            Token.SetIdToken sid -> Left (Exp.SetId sid, input1)
            _ -> Right (UnexpectedInput (show t)))

{- Parses a real interval -}
parse_interval :: Parser Exp.Set
parse_interval input = next_token input `pbind` (\(t, input1) ->
    case t of
        Token.DelimiterToken "[" -> case extract_interval input1 of
            Left p -> let (interval, endClosed) = (fst (fst p), snd (fst p))
                in case split_interval interval of
                    Left (begin, end) -> case (parse_expr begin, parse_expr end) of
                        (Left (e1, _), Left (e2, _)) -> if endClosed
                            then Left (Exp.Set ["_x0"] (Exp.SetId "R") 
                                [ Exp.Binary Exp.GE (Exp.Id "_x0") e1
                                , Exp.Binary Exp.LE (Exp.Id "_x0") e2 ], snd p)
                            else Left (Exp.Set ["_x0"] (Exp.SetId "R") 
                                [ Exp.Binary Exp.GE (Exp.Id "_x0") e1
                                , Exp.Binary Exp.L (Exp.Id "_x0") e2 ], snd p) 
                        _ -> Right IntervalFormatError
                    Right err -> Right err
            Right err -> Right err
        Token.DelimiterToken "(" -> case extract_interval input1 of
            Left p -> let (interval, endClosed) = (fst (fst p), snd (fst p))
                in case split_interval interval of
                    Left (begin, end) -> case (parse_expr begin, parse_expr end) of
                        (Left (e1, _), Left (e2, _)) -> if endClosed
                            then Left (Exp.Set ["_x0"] (Exp.SetId "R") 
                                [ Exp.Binary Exp.G (Exp.Id "_x0") e1
                                , Exp.Binary Exp.LE (Exp.Id "_x0") e2 ], snd p)
                            else Left (Exp.Set ["_x0"] (Exp.SetId "R") 
                                [ Exp.Binary Exp.G (Exp.Id "_x0") e1
                                , Exp.Binary Exp.L (Exp.Id "_x0") e2 ], snd p) 
                        _ -> Right IntervalFormatError
                    Right err -> Right err
            Right err -> Right err
        _ -> Right (UnexpectedInput (show t)))

{- Parses a set comprehension -}
parse_sliteral :: Parser Exp.Set
parse_sliteral input = next_token input `pbind` (\(t, input1) ->
    case t of
        Token.DelimiterToken "{" -> case extract_set input1 of 
            Left p -> case split_set (fst p) of
                Left (pb, condtokens) -> 
                    case extract_pb pb of
                        Left ((ptokens, base), []) -> 
                            let eparse = map_parse_expr . split_comma
                            in case (eparse ptokens, eparse condtokens) of 
                                (Left (params, []), Left (conds, [])) -> case reindex params conds of
                                    Left ((params', conds'), []) -> Left (Exp.Set (map show params') base conds', snd p)
                                    Left _ -> Right SetFormatError
                                    Right err -> Right err
                                _ -> Right SetFormatError
                        Left _ -> Right SetFormatError
                        Right err -> Right err
                Right err -> Right err
            Right err -> Right err
        _ -> Right SetFormatError)



-- parse_sb :: Parser Exp.Set
-- parse_sb input = 



{- Functionality for parsing commands starts below -}

{- Splits tokens at equals. Returns Nothing if there is no
 - assignment or an lvalue and rvalue for assignment -}
split_equals :: [Token.InputToken] -> Maybe ([Token.InputToken], [Token.InputToken])
split_equals input =
    let split_equals' [] revtokens = Nothing
        split_equals' (t : tokens) revtokens = case t of
            Token.AssignToken "=" -> Just (reverse revtokens, tokens)
            _ -> split_equals' tokens (t : revtokens)
    in split_equals' input []

{- Helper function for parsing a command -}
remove_while :: (a -> Bool) -> [a] -> [a]
remove_while f [] = []
remove_while f (l : lst) = if f l
    then remove_while f lst
    else l : lst
remove_spaces :: [Char] -> [Char]
remove_spaces input = 
    let reversed = remove_while (\l -> (l == ' ') || (l == '\t')) (reverse input)
    in remove_while (\l -> (l == ' ') || (l == '\t')) (reverse reversed)
split_spaces :: [Char] -> [[Char]]
split_spaces input = 
    let split_spaces' [] [] terms = reverse terms
        split_spaces' [] current terms = reverse (reverse current : terms)
        split_spaces' (h : t) current terms = if (h == ' ') || (h == '\t')
            then if null current
                then split_spaces' t [] terms
                else split_spaces' t [] (reverse current : terms)
            else split_spaces' t (h : current) terms
    in split_spaces' input [] []

{- Parse a builtin command 
parse_builtin :: [Char] -> Maybe D.Command
parse_builtin input = 
    let lst = (split_spaces . remove_spaces) input
    in case lst of 
        [] -> Nothing
        cmd : [] -> if cmd == "\\about"
            then Just (D.Builtin D.About)
            else if cmd == "\\bindings"
                then Just (D.Builtin D.Bindings)
                else if cmd == "\\exit"
                    then Just (D.Builtin D.Exit)
                    else if cmd == "\\help"
                        then Just (D.Builtin (D.Help 0))
                        else Nothing
        cmd : arg : [] -> if cmd == "\\help"
            then if arg == "0"
                then Just (D.Builtin (D.Help 0))
                else if arg == "1"
                    then Just (D.Builtin (D.Help 1))
                    else if arg == "2"
                        then Just (D.Builtin (D.Help 2))
                        else Nothing
            else Nothing
        _ -> Nothing -}



{- Parse user input into a command 
parse :: [Char] -> Either D.Command ParseError
parse input = case parse_builtin input of
    Just cmd -> Left cmd
    Nothing -> let lexed_input = L.lex input
        in case split_equals lexed_input of
            Nothing -> case parse_expr lexed_input of
                Left (e, _) -> Left (D.Eval e)
                Right e -> Right e
            Just (l, r) -> case (parse_expr l, parse_expr r) of
                (Left (lvalue, _), Left (rvalue, _)) -> Left (D.Assign lvalue rvalue)
                (Right e, _) -> Right e
                (_, Right e) -> Right e -}
