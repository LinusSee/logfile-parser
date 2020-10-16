module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Reset
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            model + 1

        Submit ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    Html.form []
        [ div []
            [ label []
                [ text "Type"
                , input [ placeholder "e.g. one of" ] []
                ]
            , label []
                [ text "Matching"
                , input [ placeholder "'a', 'b', 'c'" ] []
                ]
            ]
        , div []
            [ label []
                [ text "Name"
                , input [ placeholder "Loglevel oneof" ] []
                ]
            ]
        , div []
            [ button [ onClick Reset ] [ text "Reset" ]
            , button [ onClick Submit ] [ text "Submit" ]
            ]
        ]
