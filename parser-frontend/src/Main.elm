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
import Http



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success PatternFormData String


type alias PatternFormData =
    { patternType : String
    , matching : String
    , name : String
    }


type alias SampleData =
    { val1 : Int
    , val2 : String
    , val3 : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "http://localhost:8080/api/simple-string"
        , expect = Http.expectString GotDummyData
        }
    )



-- { patternType = ""
-- , matching = ""
-- , name = ""
-- }
-- { val1 = 1
-- , val2 = ""
-- , val3 = ""
-- }
-- UPDATE


type Msg
    = GotDummyData (Result Http.Error String)
    | ChangePatternType String
    | ChangeMatching String
    | ChangeName String
    | Reset
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDummyData result ->
            case result of
                Ok data ->
                    ( Success
                        { patternType = ""
                        , matching = ""
                        , name = ""
                        }
                        data
                    , Cmd.none
                    )

                Err error ->
                    Debug.log (Debug.toString error) ( Failure, Cmd.none )

        ChangePatternType newTypeContent ->
            case model of
                Failure ->
                    ( Failure, Cmd.none )

                Loading ->
                    ( Failure, Cmd.none )

                Success formData loadedData ->
                    ( Success { formData | patternType = newTypeContent } "ChangePatternType", Cmd.none )

        ChangeMatching newMatchingContent ->
            case model of
                Failure ->
                    ( Failure, Cmd.none )

                Loading ->
                    ( Failure, Cmd.none )

                Success formData loadedData ->
                    ( Success { formData | matching = newMatchingContent } "ChangeMatching", Cmd.none )

        ChangeName newName ->
            case model of
                Failure ->
                    ( Failure, Cmd.none )

                Loading ->
                    ( Failure, Cmd.none )

                Success formData loadedData ->
                    ( Success { formData | name = newName } "ChangeName", Cmd.none )

        Reset ->
            ( Success
                { patternType = ""
                , matching = ""
                , name = ""
                }
                "Reset"
            , Cmd.none
            )

        Submit ->
            ( Success
                { patternType = ""
                , matching = ""
                , name = "Submitted"
                }
                "Submit"
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            div [] [ text "Failed to load data" ]

        Loading ->
            div [] [ text "Loading..." ]

        Success formData loadedData ->
            div []
                [ h2 [] [ text "Create specialized parsers" ]
                , div []
                    [ label []
                        [ text "Type"
                        , select [ value formData.patternType, onInput ChangePatternType ]
                            [ option [ value "asdf1", selected (formData.patternType == "asdf1") ] [ text "Option 1" ]
                            , option [ value "qwert2", selected (formData.patternType == "qwert2") ] [ text "Option 2" ]
                            , option [ value "yxcv3", selected (formData.patternType == "yxcv3") ] [ text "Option 3" ]
                            ]

                        --, input [ placeholder "e.g. one of", value model.patternType, onInput ChangePatternType ] []
                        ]
                    , label []
                        [ text "Matching"
                        , input [ placeholder "'a', 'b', 'c'", value formData.matching, onInput ChangeMatching ] []
                        ]
                    ]
                , div []
                    [ label []
                        [ text "Name"
                        , input [ placeholder "Loglevel oneof", value formData.name, onInput ChangeName ] []
                        ]
                    ]
                , div []
                    [ button [ onClick Reset ] [ text "Reset" ]
                    , button [ onClick Submit ] [ text "Submit" ]
                    ]
                , text ("Loaded this string: " ++ loadedData)
                ]
