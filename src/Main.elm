module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Type exposing (..)
import Url



-- Main Function --


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subs
        , update = update
        , view = view

        -- Stuff --
        , onUrlChange = NewUrl
        , onUrlRequest = Link
        }



-- Model & Types --


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    }


type Msg
    = NewUrl Url.Url
    | Link Browser.UrlRequest



-- Initialization --


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url, Cmd.none )



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    Sub.none



-- Update Function --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Link _ ->
            ( model, Cmd.none )

        NewUrl _ ->
            ( model, Cmd.none )



-- View Function --


view : Model -> Browser.Document Msg
view model =
    { title = "Evaλ"
    , body =
        [ Element.layout
            [ width fill
            , height fill
            , padding 50
            , Font.color <| rgb 1 0.8 0.3
            , Font.family [ Font.monospace ]
            , Font.size 40
            , Bg.color <| rgb 0 0 0
            ]
          <|
            column [ spacing 40 ]
                [ el [] <| text "e ::= x | λx.e | e e"
                , el [] <| text "e ::= x | λx:τ.e | e e | c"
                , el [] <| text "Howdy"
                ]
        ]
    }



{- module Main exposing (..)

   import Browser
   import Browser.Events as Events
   import Browser.Navigation as Nav
   import Dict
   import Element exposing (..)
   import Element.Background as Bg
   import Element.Border as Border
   import Element.Font as Font
   import Element.Input as Input
   import Html
   import Lang exposing (..)
   import Parser as P exposing ((|.), (|=))
   import Set
   import Url


   main : Program () Model Msg
   main =
       Browser.sandbox
           { init = init
           , view = view
           , update = update
           }



   -- Model --


   type alias Model =
       { env : Lang.Env
       , expr : String
       , currentDef : String
       , err : String
       }


   type Msg
       = CurrentDef String
       | AddDef String
       | DelDef String
       | EditDef String


   init : Model
   init =
       Model Dict.empty "" "" ""



   -- Update --


   update : Msg -> Model -> Model
   update msg model =
       case msg of
           CurrentDef str ->
               let
                   parsedName =
                       P.run (parseChar Char.isUpper) str

                   -- TODO: Parse out name and convert expression to AST.
                   -- Also needs parsedDef function!
               in
               case parsedName of
                   Ok out ->
                       { model
                           | currentDef = String.replace "\\" "λ" str
                           , err = ""

                           -- FIXME: currentDef
                       }

                   Err _ ->
                       { model
                           | currentDef = String.replace "\\" "λ" str
                           , err =
                               "Variables must be named with a Capital Letter!"

                           -- FIXME: currentDef
                       }

           AddDef def ->
               let
                   outDef =
                       stringToDef def
               in
               case outDef of
                   Def name xs tr ->
                       if model.err == "" then
                           { model
                               | env = Dict.insert name ( def, outDef ) model.env
                               , currentDef = ""
                           }
                       else
                           model

                   ErrorDef ->
                       model

           DelDef key ->
               { model | env = Dict.remove key model.env }

           EditDef val ->
               { model
                   | currentDef = val
               }



   -- View --


   view : Model -> Html.Html Msg
   view model =
       Element.layout
           [ width fill
           , height fill
           , Font.family [ Font.monospace ]
           , Font.color <| inv <| rgb 1 1 1
           , Bg.color <| inv <| rgb 0 0 0
           ]
       <|
           column [ width fill, height fill ]
               [ el
                   [ padding 20
                   , centerX
                   , Bg.color <| inv <| rgb 0 0.1 0.2
                   , width fill
                   , Font.center
                   , Font.size 31
                   ]
                 <|
                   text <|
                       "λ-Calculus / Combinators"
               , row
                   [ width fill
                   , padding 10
                   , spacing 10
                   ]
                   [ Input.text
                       [ Bg.color <| inv <| rgb 0.1 0.1 0.1
                       , width fill
                       ]
                       { onChange = \x -> CurrentDef x
                       , text = model.currentDef
                       , placeholder =
                           Just <|
                               Input.placeholder [] <|
                                   text "Define a Function ('\\' => 'λ')"
                       , label = Input.labelHidden "New lambda definition."
                       }
                   , Input.button
                       [ padding 12
                       , Font.size 20
                       , Bg.color <| inv <| rgb 0 0.3 0.6
                       , Border.rounded 5
                       ]
                       { onPress =
                           Just <|
                               AddDef model.currentDef
                       , label = text "Add"
                       }
                   ]
               , if model.err /= "" then
                   paragraph [ padding 10, Font.color <| inv <| rgb 0.8 0 0 ]
                       [ text model.err ]
                 else
                   el [] none
               , column
                   [ width fill
                   , padding 10
                   , spacing 5
                   ]
                   [ el [] <| text "Environment:"
                   , column
                       [ padding 10
                       , spacing 5
                       , width fill
                       , height fill
                       , Bg.color <|
                           inv <|
                               rgb 0.05 0.05 0.05
                       ]
                       (model.env
                           |> Dict.toList
                           |> List.map displayPair
                       )
                   , el [ height <| px 20 ] none
                   , row [ width fill ]
                       [ el [] <| text "Evaluator:"
                       , Input.button
                           [ padding 12
                           , Font.size 20
                           , Bg.color <| inv <| rgb 0.2 0.4 0
                           , Border.rounded 5
                           , alignRight
                           ]
                           { onPress =
                               Just <|
                                   AddDef model.currentDef
                           , label = text "Next Step"
                           }
                       ]
                   , column
                       [ padding 10
                       , spacing 2
                       , width fill
                       , height fill
                       , Bg.color <|
                           inv <|
                               rgb 0.05 0.05 0.05
                       ]
                       [ el [] <|
                           text "(((S (K (S I)) K) x) y)"
                       , el [ Font.color <| inv <| rgb 0.8 0 0 ] <|
                           text " S=λ └─a─────┘ b  c."

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
                       , el [ Font.color <| inv <| rgb 0.8 0 0 ] <|
                           text "     ┌─a─────┐ c  (b c)"
                       , el [] <|
                           text "  ((((K (S I)) x) (K x)) y)"
                       , el [ Font.color <| inv <| rgb 0.8 0 0 ] <|
                           text "    K=λ └─a─┘  b."
                       , el [ Font.color <| inv <| rgb 0.8 0 0 ] <|
                           text "        ┌─a─┐"
                       , el [] <|
                           text "      (((S I) (K x)) y)"
                       , el [ Font.color <| inv <| rgb 0.8 0 0 ] <|
                           text "       S=λ a  └─b─┘  c."
                       , el [ Font.color <| inv <| rgb 0.8 0 0 ] <|
                           text "           a c  (┌─b─┐ c)"
                       , el [] <|
                           text "         ((I y) ((K x) y))"
                       , el [ Font.color <| inv <| rgb 0.8 0 0 ] <|
                           text "         I=λ a."
                       , el [ Font.color <| inv <| rgb 0.8 0 0 ] <|
                           text "             a"
                       , el [] <|
                           text "            (y ((K x) y))"
                       , el [ Font.color <| inv <| rgb 0.8 0 0 ] <|
                           text "               K=λ a  b."
                       , el [ Font.color <| inv <| rgb 0.8 0 0 ] <|
                           text "                   a"
                       , el [] <|
                           text "                (y x)"
                       ]
                   ]
               ]


   displayPair : ( String, ( String, Def ) ) -> Element Msg
   displayPair ( key, ( valStr, valExpr ) ) =
       row [ spacing 10 ]
           [ Input.button
               [ padding 5
               , Font.size 20
               , Bg.color <| inv <| rgb 0 0.3 0.6
               , Border.rounded 5
               ]
               { onPress = Just <| EditDef valStr, label = text "Edit" }
           , Input.button
               [ padding 5
               , Font.size 20
               , Bg.color <| inv <| rgb 0.8 0 0
               , Border.rounded 5
               ]
               { onPress = Just <| DelDef key, label = text "Delete" }
           , el [ Font.color <| inv <| rgb 0.7 0.7 0.7 ] <|
               text <|
                   defToString valExpr
           ]


   {-| Set this to True to Invert Colors.
   -}
   isInverted : Bool
   isInverted =
       --True
       False


   inv : Color -> Color
   inv color =
       case isInverted of
           True ->
               let
                   rgb =
                       color |> toRgb
               in
               { rgb
                   | red = 1.0 - rgb.red
                   , green = 1.0 - rgb.green
                   , blue = 1.0 - rgb.blue
               }
                   |> fromRgb

           False ->
               color



   --
   -- └┘ ─ ┬ ┴ ┼ ┌┐ │ ╱ ╳ ╲ ┳ ┗ ╂ ┻ ┏ ┓ ━
   --
   --text "λ ───┬─── ┬ ┬ ┬"
   --text "     │    └─┼┐└───┐"
   --text "     │    ┌─┴┼─┐  │"
   --text "  ───┴─── ┴  ┴ ┴  ┴"
-}
