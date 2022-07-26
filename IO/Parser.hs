{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File for parsing input -}
module IO.Parser where
import qualified ExpData.Expression.Type as E
import qualified ExpData.Expression.Operator as O
import qualified IO.Command.Type as D
import qualified IO.Utils.Lexer as L
import qualified IO.Utils.Token as T





{- Error type for helpful feedback -}
data ParseError
    = FailedToBuildExpression
    | MismatchedBrackets
    | UnexpectedEndOfInput
    | ExpectedExpression [Char]
    | UnexpectedInput [Char]
    deriving (Show, Read)

{- Parser type for simplpifying type signatures -}
type ParseResult t = Either (t, [T.InputToken]) ParseError
type Parser t = [T.InputToken] -> ParseResult t



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
 - Plays nice with bind and fails -}
next_token :: Parser T.InputToken
next_token (t : input1) = Left (t, input1)
next_token _ = Right UnexpectedEndOfInput

{- Parses an operator with the appropriate precedence
 - Used in parse_atail through parse_dtail to maintain
 - the order of operaions -}
parse_op_prec :: Int -> Parser O.Op
parse_op_prec n [] = Right UnexpectedEndOfInput
parse_op_prec n (t : input) = case t of
    T.OpToken o ->
        if O.operator_precedence (O.get_op o) == n
        then Left (O.get_op o, input)
        else Right (UnexpectedInput (show t))
    _ -> Right (UnexpectedInput (show t))

{- Helper function for parse_a through parse_d. Takes
 - the results of parse_atail through parse_dtail and
 - builds out the expression tree -}
build_tail :: ([E.Expression], [O.Op]) -> Either E.Expression ParseError
build_tail ([], _) = Right FailedToBuildExpression
build_tail ([e], _) = Left e
build_tail (_, []) = Right FailedToBuildExpression
build_tail (e1 : e2 : exps, o : ops) =
    build_tail (E.Binary o e1 e2 : exps, ops)

{- Extracts a parenthetical for nested parsing.
 - Used for function call arguments as well as
 - simple parenthetical expressions -}
extract_parenthetical :: Parser [T.InputToken]
extract_parenthetical input =
    let extract_parenthetical' depth [] revtokens = Right MismatchedBrackets
        extract_parenthetical' depth (t : tokens) revtokens = case t of
            T.DelimiterToken "(" ->
                extract_parenthetical' (depth + 1) tokens (t : revtokens)
            T.DelimiterToken ")" ->
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
separate_args :: Parser [E.Expression]
separate_args input =
    let separate_args' depth (t : tokens) revtokens revargs = case t of
            T.DelimiterToken "(" ->
                separate_args' (depth + 1) tokens (t : revtokens) revargs
            T.DelimiterToken ")" ->
                if depth == 0
                then Right MismatchedBrackets
                else separate_args' (depth - 1) tokens (t : revtokens) revargs
            T.DelimiterToken "," ->
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



{- The main parser functionality starts below -}

{- Rule: Expr -> AAtail -}
parse_expr :: Parser E.Expression
parse_expr input = case parse_operand 0 input of
    Left (expr, []) -> Left (expr, [])
    Left (expr, t : tokens) -> case t of
        T.OpToken o -> Right (ExpectedExpression (show t))
        _ -> Right (UnexpectedInput (show t))
    Right err -> Right err

{- Rules:
 - Expr -> AAtail
 - A -> BBtail
 - B -> CCtail
 - C -> DDtail -}
parse_operand :: Int -> Parser E.Expression
parse_operand prec input = if prec < 4
    then
        parse_operand (prec + 1) input `pbind` (\(e, input1) ->
        parse_optail (prec + 1) e input1 `pbind` (\(pairs, input2) ->
        build_tail pairs `pbind` (\exp -> Left (exp, input2))))
    else
        parse_d input

{- Rules:
 - Atail -> XAAtail | Empty
 - Btail -> YBBtail | Empty
 - Ctail -> ZCCtail | Empty
 - Dtail -> \^DDtail | Empty -}
parse_optail :: Int -> E.Expression -> Parser ([E.Expression], [O.Op])
parse_optail prec exp input =
    let parse_optail' [] (exps, ops) = Left ((reverse exps, reverse ops), [])
        parse_optail' input (exps, ops) =
            parse_op_prec prec input `pbind` (\(o, input1) ->
            parse_operand prec input1 `pbind` (\(e, input2) ->
            parse_optail' input2 (e : exps, o : ops))) `pfails` (\() ->
            Left ((reverse exps, reverse ops), input))
    in parse_optail' input ([exp], [])

{- Rule: D -> \-D | F -}
parse_d :: Parser E.Expression
parse_d input =
    next_token input `pbind` (\(t, input1) ->
    case t of
        T.OpToken "-" ->
            parse_d input1 `pbind` (\(e, input2) ->
            Left (E.Negate e, input2))
        _ -> parse_f input)

{- Rule: F -> IdOrCall | Num | Paren -}
parse_f :: Parser E.Expression
parse_f input =
    parse_id_or_call input `pfails` (\() ->
    parse_num input `pfails` (\() ->
    parse_parens input))

{- Helper functions for parse_id_or_call -}
parse_id :: Parser E.Expression
parse_id input =
    next_token input `pbind` (\(t, input1) ->
    case t of
        T.IdToken id -> Left (E.Id id, input1)
        _ -> Right (UnexpectedInput (show t)))
parse_call :: Parser [E.Expression]
parse_call input =
    next_token input `pbind` (\(t, input1) ->
    case t of
        T.DelimiterToken "(" ->
            extract_parenthetical input1 `pbind` (\(parens, input2) ->
            separate_args parens `pbind` (\(args, _) ->
            Left (args, input2)))
        _ -> Right (UnexpectedInput (show t)))

{- Rule: IdOrCall -> Id (Args | Empty) -}
parse_id_or_call :: Parser E.Expression
parse_id_or_call input =
    parse_id input `pbind` (\(eid, input1) ->
        case eid of
            E.Id id -> parse_call input1 `pbind` (\(args, input2) ->
                Left (E.FCall id args, input2)) `pfails` (\() -> Left (eid, input1))
            _ -> Right (UnexpectedInput (show eid)))

{- Parses a number or parenthetical if
 - parse_id_or_call fails -}
parse_num :: Parser E.Expression
parse_num input =
    next_token input `pbind` (\(t, input1) ->
    case t of
        T.NumLiteralToken num -> Left (E.Num num, input1)
        _ -> Right (UnexpectedInput (show t)))
parse_parens :: Parser E.Expression
parse_parens input =
    next_token input `pbind` (\(t, input1) ->
    case t of
        T.DelimiterToken "(" ->
            extract_parenthetical input1 `pbind` (\(parens, input2) ->
            parse_expr parens `pbind` (\(exp, _) ->
            Left (E.Parenthetical exp, input2)))
        _ -> Right (UnexpectedInput (show t)))



{- Functionality for parsing commands starts below -}

{- Splits tokens at equals. Returns Nothing if there is no
 - assignment or an lvalue and rvalue for assignment -}
split_equals :: [T.InputToken] -> Maybe ([T.InputToken], [T.InputToken])
split_equals input =
    let split_equals' [] revtokens = Nothing
        split_equals' (t : tokens) revtokens = case t of
            T.DelimiterToken "=" -> Just (reverse revtokens, tokens)
            _ -> split_equals' tokens (t : revtokens)
    in split_equals' input []

{- Helper function for parsing a command -}
remove_while :: (a -> Bool) -> [a] -> [a]
remove_while f [] = []
remove_while f (l : lst) = if f l
    then remove_while f lst
    else l : lst

{- Parse user input into a command -}
parse_input :: [Char] -> Either D.Command ParseError
parse_input input
    | remove_while (\l -> (l == ' ') && (l == '\t')) input == "\\about" = Left (D.Builtin D.About)
    | remove_while (\l -> (l == ' ') && (l == '\t')) input == "\\bindings" = Left (D.Builtin D.Bindings)
    | remove_while (\l -> (l == ' ') && (l == '\t')) input == "\\exit" = Left (D.Builtin D.Exit)
    | remove_while (\l -> (l == ' ') && (l == '\t')) input == "\\help" = Left (D.Builtin D.Help)
    | otherwise =
        let lexed_input = L.lex input
        in case split_equals lexed_input of
            Nothing -> case parse_expr lexed_input of
                Left (e, _) -> Left (D.Eval e)
                Right e -> Right e
            Just (l, r) -> case (parse_expr l, parse_expr r) of
                (Left (lvalue, _), Left (rvalue, _)) -> Left (D.Assign lvalue rvalue)
                (Right e, _) -> Right e
                (_, Right e) -> Right e
