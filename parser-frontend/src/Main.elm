module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, h2, input, label, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { patternType : String
    , matching : String
    , name : String
    }


init : Model
init =
    { patternType = ""
    , matching = ""
    , name = ""
    }



-- UPDATE


type Msg
    = ChangePatternType String
    | ChangeMatching String
    | ChangeName String
    | Reset
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangePatternType newTypeContent ->
            { model | patternType = newTypeContent }

        ChangeMatching newMatchingContent ->
            { model | matching = newMatchingContent }

        ChangeName newName ->
            { model | name = newName }

        Reset ->
            { patternType = ""
            , matching = ""
            , name = ""
            }

        Submit ->
            { patternType = ""
            , matching = ""
            , name = "Submitted"
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Create specialized parsers" ]
        , div []
            [ label []
                [ text "Type"
                , select [ value model.patternType, onInput ChangePatternType ]
                    [ option [ value "asdf1", selected (model.patternType == "asdf1") ] [ text "Option 1" ]
                    , option [ value "qwert2", selected (model.patternType == "qwert2") ] [ text "Option 2" ]
                    , option [ value "yxcv3", selected (model.patternType == "yxcv3") ] [ text "Option 3" ]
                    ]

                --, input [ placeholder "e.g. one of", value model.patternType, onInput ChangePatternType ] []
                ]
            , label []
                [ text "Matching"
                , input [ placeholder "'a', 'b', 'c'", value model.matching, onInput ChangeMatching ] []
                ]
            ]
        , div []
            [ label []
                [ text "Name"
                , input [ placeholder "Loglevel oneof", value model.name, onInput ChangeName ] []
                ]
            ]
        , div []
            [ button [ onClick Reset ] [ text "Reset" ]
            , button [ onClick Submit ] [ text "Submit" ]
            ]
        ]
