module Eval exposing (..)

{-| This module for composing steps in a nano-pass evaluator or compiler.
-}

import Dict


{-| Should there be a Cons? Or different Expr types?
-}
type Expr
    = Atom String
    | Var UpdateFlag String
    | Apply UpdateFlag Expr Expr
    | Lambda String Expr
    | Empty


apply : Expr -> Value Expr
apply expr =
    case expr of
        Apply flag (Lambda var body) appliedExpr ->
            Value (\() -> Atom var) emptyEnv

        _ ->
            Value (\() -> Empty) emptyEnv


{-| Sub-trees in the AST need to be tagged so as not to follow them if they
are already fully evaluated.
-}
type UpdateFlag
    = Updatable
    | Attempted
    | Finalized


{-| Value compared to a Store

type Store s a = Store s (s -> a)

FIXME: The stored function should probably be a transformation of an
expression that is triggered by an empty tuple.

Maybe using a function like:

lazy : (Expr -> Expr) -> (Expr -> () -> Expr)

-}
type Value expr
    = Value (() -> expr) (Env expr)


type alias Env expr =
    Dict.Dict String expr



-- Functor Interface --


{-| Just transforms the Expression in a Value without consulting the Env.
-}
map : (Expr -> Expr) -> Value Expr -> Value Expr
map fun val =
    case val of
        Value exprFun env ->
            Value (exprFun >> fun) env



-- Applicative Interface --
{- Applies a function wrapped in a Value to the Expr in a Value.


   TODO: Consider whether a Dict String (Expr -> Expr) is useful.
   Maybe for converting function calls to lambda expressions.

   apply : Value (Expr -> Expr) -> Value Expr -> Value Expr
   apply funVal val =
       case funVal of
           Value fun _ ->
               case val of
                   Value expr env ->
                       Value (fun expr) env
-}
-- Monadic Interface --


{-| Wraps an Expr in a Value with an empty Env.
-}
wrap : Expr -> Value Expr
wrap expr =
    Value (\() -> expr) emptyEnv


{-| For composing functions that add entries to the Env.
-}
bind : (Expr -> Value Expr) -> Value Expr -> Value Expr
bind fun val =
    case val of
        Value exprFun env ->
            let
                partialVal =
                    fun <| exprFun ()
            in
            case partialVal of
                Value partExprFun partEnv ->
                    Value partExprFun <| Dict.union env partEnv



{-
   {- Maybe TODO: join : Value (Value Expr) -> Value Expr -}
   -- Comonadic Interface --


   {- Retrieve the Expr from a Value.
   -}
-}


extract : Value Expr -> Expr
extract val =
    case val of
        Value exprFun _ ->
            exprFun ()



{-
   {- Wraps a Value in another Value

   TODO: Consider whether or not this is a bad idea.

   -}
   duplicate : Value Expr -> Value (Value Expr)
   duplicate val =
       case val of
           Value exprFun env ->
               Value (Value expr env) emptyEnv


   {- For composing functions converts a Value to an Expr

   TODO: Consider whether there are ever times in evaluation where this is
   useful.

   -}
   extend : (Value Expr -> Expr) -> Value Expr -> Value Expr
   extend fun val =
       let
           newExpr =
               fun val
       in
       case val of
           Value expr env ->
               Value newExpr env



   {- FIXME: Find a concise method for manipulating Expressions.

      case expr of
        Atom str ->
        Var str ->
          let
            newExpr =
              case Dict.get str env of
                Just ex -> ex
                Nothing -> Var str
          in
            Value newExpr env

        Apply ex1 ex2 ->
          case ex1 of

        Lambda str ex ->
   -}
-}


{-| Produce an empty Environment.
-}
emptyEnv : Env a
emptyEnv =
    Dict.empty



{- FIXME: Output should probably be a different type than input.
   There needs to at least be some way to mark what has already been evaluated.

   TODO: Consider a linear representation of output...
   Also, multiple differnet passes.

     - Look at Elm compliler method of evaluation.
     - eval should probably either be monadic or applicative.

   eval : ( Env, Expr ) -> ( Env, Expr )
   eval ( env, expr ) =
   case expr of
   Atom a ->
   ( env, expr )

           Var a ->
               ( env, Maybe.withDefault Empty <| Dict.get a env )

           Apply a b ->
               let
                   ( e1, x1 ) =
                       eval ( env, a )
               in
               let
                   -- FIXME: What should output look like?
                   ( e2, x2 ) =
                       eval ( env, b )
               in
               eval ( Dict.union e1 e2, Apply x1 x2 )

           Lam a ->
               -- FIXME: What should output look like?
               ( env, expr )

           Empty ->
               -- FIXME: What should output look like?
               ( env, expr )

-}
