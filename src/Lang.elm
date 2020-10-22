module Lang exposing (..)

{-| This module provides the AST, Parsers, Evaluator functions for a lambda
calculus-based language for learning functional programming.

TODO: only expose the relevant API for using this interpreter.
Consider the possibility of modularizing the language definition to allow for
nano-pass compilation (or cross-compilation between languages).

  - (define-pass name inputLang -> outputLang transform*)


# AST

@docs Expr, Env, Loc


# Parsers

@docs parseExpr

-}

import Char
import Dict
import Parser as P exposing ((|.), (|=))
import Set


-- AST --


{-| This is the fundamental type for this language's Abstract Syntax Tree.
-}
type Expr
    = Atom String
    | Var String
    | Apply Expr Expr
    | Lam Expr Expr
    | Empty


{-| An environment is needed to provide context for any part of a code-base to
deal with abstractions.

TODO: Consider how unions and intersections in a Dict can relate to algebraic
type construction and structural convensions for semantically equivalent
definitions.

-}
type alias Env =
    Dict.Dict String ( String, Def )


{-| This type is for storing the appropriate definitions in an Environment.
-}
type Def
    = Def String (List String) Expr
    | ErrorDef



-- Parsers --


{-| Takes any function from a Char to a Bool and return a parser that checks for
that criteria.
-}
parseChar : (Char -> Bool) -> P.Parser String
parseChar f =
    P.getChompedString <|
        P.succeed ()
            |. P.chompIf (\c -> f c)


{-| An atom is defined as any lowercase letter followed by zero or more upper or
lowercase letters.
-}
atomParser : P.Parser Expr
atomParser =
    P.succeed Atom
        |= P.variable
            { start = Char.isLower
            , inner = Char.isAlpha
            , reserved = Set.empty
            }


{-| An variable is defined as any uppercase letter followed by zero or more
upper or lowercase letters.
-}
varParser : P.Parser Expr
varParser =
    P.succeed Var
        |= P.variable
            { start = Char.isUpper
            , inner = Char.isAlpha
            , reserved = Set.empty
            }


{-| Function application is defined by two expressions wrapped in parentheses.
-}
applyParser : P.Parser Expr
applyParser =
    P.succeed Apply
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= exprParser
        |. P.spaces
        |= exprParser
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces


{-| Parser for recursively translating expressions into appropriate AST.
-}
parseExpr : P.Parser Expr
parseExpr =
    P.loop [] <|
        \revInputs ->
            P.oneOf
                [ P.succeed (\input -> P.Loop (input :: revInputs))
                    |= exprParser
                    |. P.spaces
                , P.succeed ()
                    |> P.map
                        (\_ ->
                            P.Done
                                (List.reverse revInputs |> listToExpr)
                        )
                ]


{-| Takes a list of Expressions and applies them together in a left-associative
manner.
-}
listToExpr : List Expr -> Expr
listToExpr exprList =
    case exprList of
        x :: y :: xs ->
            listToExpr (Apply x y :: xs)

        x :: [] ->
            x

        [] ->
            Empty


{-| A parser for choosing which Expression parser should be used
-}
exprParser : P.Parser Expr
exprParser =
    P.oneOf
        [ P.lazy (\_ -> applyParser)
        , P.lazy (\_ -> atomParser)
        , P.lazy (\_ -> varParser)
        , P.lazy (\_ -> parseLam)
        ]


{-| Parses function definitions.
-}
parseDef : P.Parser Def
parseDef =
    P.succeed Def
        |= P.variable
            { start = Char.isUpper
            , inner = Char.isAlpha
            , reserved = Set.empty
            }
        |. P.spaces
        |= parseInputs
        |. P.symbol "="
        |. P.spaces
        |= parseExpr


{-| Parses a list of inputs to a function.
-}
parseInputs : P.Parser (List String)
parseInputs =
    P.loop [] <|
        \revInputs ->
            P.oneOf
                [ P.succeed (\input -> P.Loop (input :: revInputs))
                    |= P.variable
                        { start = Char.isLower
                        , inner = Char.isAlpha
                        , reserved = Set.empty
                        }
                    |. P.spaces
                , P.succeed ()
                    |> P.map (\_ -> P.Done (List.reverse revInputs))
                ]


{-| Returns a string that represents a function definition as a lambda
expression.
-}
defToString : Def -> String
defToString def =
    case def of
        Def name list tr ->
            name ++ " = " ++ defToLambda list tr

        ErrorDef ->
            "ERROR!"


{-| Turns a list of inputs into lambda abstraction notation.
-}
defToLambda : List String -> Expr -> String
defToLambda list tr =
    case list of
        x :: xs ->
            "λ" ++ x ++ "." ++ defToLambda xs tr

        [] ->
            exprToString tr


{-| Converts the AST for an expression into a string representation.
-}
exprToString : Expr -> String
exprToString tr =
    case tr of
        Apply a b ->
            "(" ++ exprToString a ++ " " ++ exprToString b ++ ")"

        Lam x e ->
            "λ" ++ exprToString x ++ "." ++ exprToString e

        Atom a ->
            a

        Var a ->
            a

        Empty ->
            ""


{-| Converts a string to a Definition, dealing with error cases.
-}
stringToDef : String -> Def
stringToDef str =
    let
        def =
            P.run parseDef str
    in
    case def of
        Ok d ->
            d

        Err _ ->
            ErrorDef


{-| A parser for Lambda Expressions.
-}
parseLam : P.Parser Expr
parseLam =
    P.succeed Lam
        |. P.oneOf [ P.symbol "λ", P.symbol "\\" ]
        |. P.spaces
        |= atomParser
        |. P.spaces
        |. P.symbol "."
        |. P.spaces
        |= parseExpr



-- Evaluator --
{-
   {-| Apply should invoke functions in an AST, looking up definitions in the
   environment.
   -}
   apply : Env -> Expr -> Expr -> ( Env, Expr )
   apply env expr1 expr2 =
       case expr1 of
           Atom atom ->
               Apply (Atom atom) (eval env expr2)

           Var name ->
               let
                   var =
                       Dict.get name env
               in
               case var of
                   Just x ->
                       Apply (x (eval env expr2))

                   Nothing ->
                       Apply (Atom name (eval env expr2))

           -- FIXME! All cases need to be fixed!
           Apply a b ->
               Apply a b

           Lam a b ->
               Lam a b

           Empty ->
               Empty
-}
{- type Expr
   = Atom String
   | Var String
   | Apply Expr Expr
   | Lam Expr Expr
   | Empty

   -- FIXME: Let bindings add scoped environment
   -- Should let bindings be of Expr type, or separate?
   -- Probably together.
   -- Consider literals and primitive functions.



   type alias Env =
       Dict.Dict String Expr


   type alias Loc =
       { begin : Int, end : Int }


   type Expr
       = Atom String
       | Thunk Expr
       | Var String (Maybe Expr)
       | Lam (List String) Expr
         -- | LocExpr Loc Expr
       | App Expr Expr
-}


{-| The eval function should recursively evaluate an Expression based on an
Environment.

FIXME: The use of lazy evaluation should be considered here. Since this is meant
to be a step-by-step evaluator, it should be able to leave everything but the
next logical step in an unevaluated state.

  - Lazy evaluation made explicit through the STG machine.

  - There should be a helper function that searches out the next step and only
    transforms that in the AST.

-}
eval : Env -> Expr -> Expr
eval env expr =
    case expr of
        Atom str ->
            Atom str

        Var str ->
            case Dict.get str env of
                Just ( _, ast ) ->
                    case ast of
                        Def _ _ e ->
                            -- FIXME: Make sure the resulting Expr is right.
                            e

                        ErrorDef ->
                            -- TODO: Retain undefined variables without
                            -- reevaluating.
                            Empty

                Nothing ->
                    Empty

        Apply a b ->
            -- FIXME: Return Thunks if not lazy iterations of eval.
            -- Sub-evals may need to union env and sub-envs.
            Apply (eval env a) (eval env b)

        Lam a b ->
            -- FIXME: a should be a name added to env if lambda is applied.
            Empty

        Empty ->
            Empty



-- TODO: Dynamically generate this sort of annotation
-- from AST.
--
-- 1. Variables in lambda expressions should only show
--    line drawing bars when wrapped in parentheses.
-- 2. Otherwise, the lambda input name should be shown
--    directly under the first letter of the input.
-- 3. Ideally, unnecessary parentheses should be shown
--    in a different color than necessary ones.
-- 4. Spacing should be consistent, and lambda outputs
--    should begin directly under the beginning of the
--    first input in the expression with unused
--    components shrinking inward to maintain consistent
--    spacing.
-- 5. The layout of annotations should be generated
--    either from the input expression or co-generated
--    with the display format of the AST to line up
--    properly.
-- 6. Adding color-coding to matching parentheses might
--    be helpful.
--
--
-- └┘ ─ ┬ ┴ ┼ ┌┐ │ ╱ ╳ ╲ ┳ ┗ ╂ ┻ ┏ ┓ ━
--
--text "λ ───┬─── ┬ ┬ ┬"
--text "     │    └─┼┐└───┐"
--text "     │    ┌─┴┼─┐  │"
--text "  ───┴─── ┴  ┴ ┴  ┴"
