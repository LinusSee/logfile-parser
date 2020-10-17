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
import Json.Decode exposing (Decoder, field, int, map3, string)



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
    | Success PatternFormData SampleData


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
        { url = "http://localhost:8080/api/sample"
        , expect = Http.expectJson GotDummyData sampleDataDecoder
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
    = GotDummyData (Result Http.Error SampleData)
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
                        { patternType = "asdf1"
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
                    ( Success { formData | patternType = newTypeContent }
                        loadedData
                    , Cmd.none
                    )

        ChangeMatching newMatchingContent ->
            case model of
                Failure ->
                    ( Failure, Cmd.none )

                Loading ->
                    ( Failure, Cmd.none )

                Success formData loadedData ->
                    ( Success { formData | matching = newMatchingContent }
                        loadedData
                    , Cmd.none
                    )

        ChangeName newName ->
            case model of
                Failure ->
                    ( Failure, Cmd.none )

                Loading ->
                    ( Failure, Cmd.none )

                Success formData loadedData ->
                    ( Success { formData | name = newName }
                        loadedData
                    , Cmd.none
                    )

        Reset ->
            ( Success
                { patternType = "qwert2"
                , matching = ""
                , name = ""
                }
                { val1 = 1
                , val2 = ""
                , val3 = ""
                }
            , Cmd.none
            )

        Submit ->
            ( Success
                { patternType = "yxcv3"
                , matching = ""
                , name = "Submitted"
                }
                { val1 = 1
                , val2 = ""
                , val3 = ""
                }
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
                , text ("Loaded this string: " ++ loadedData.val2)
                ]



-- HTTP


sampleDataDecoder : Decoder SampleData
sampleDataDecoder =
    map3 SampleData
        (field "dummy1" int)
        (field "dummy2" string)
        (field "dummy3" string)
