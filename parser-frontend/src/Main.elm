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


type FormChanged
    = ChangePatternType
    | ChangeMatching
    | ChangeName


type Msg
    = GotDummyData (Result Http.Error SampleData)
      --| ChangePatternType String
      --| ChangeMatching String
      --| ChangeName String
    | ChangeForm FormChanged String
    | Reset
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDummyData result ->
            case result of
                Ok data ->
                    ( Success
                        { patternType = "oneOf"
                        , matching = ""
                        , name = ""
                        }
                        data
                    , Cmd.none
                    )

                Err error ->
                    Debug.log (Debug.toString error) ( Failure, Cmd.none )

        ChangeForm field newContent ->
            case model of
                Failure ->
                    ( Failure, Cmd.none )

                Loading ->
                    ( Failure, Cmd.none )

                Success formData loadedData ->
                    case field of
                        ChangePatternType ->
                            ( Success { formData | patternType = newContent }
                                loadedData
                            , Cmd.none
                            )

                        ChangeMatching ->
                            ( Success { formData | matching = newContent }
                                loadedData
                            , Cmd.none
                            )

                        ChangeName ->
                            ( Success { formData | name = newContent }
                                loadedData
                            , Cmd.none
                            )

        Reset ->
            ( Success
                { patternType = "oneOf"
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
                { patternType = "oneOf"
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
                        , select [ value formData.patternType, onInput (ChangeForm ChangePatternType) ]
                            [ option [ value "oneOf", selected (formData.patternType == "oneOf") ] [ text "One Of" ]
                            , option [ value "date", selected (formData.patternType == "date") ] [ text "Date" ]
                            , option [ value "time", selected (formData.patternType == "time") ] [ text "Time" ]
                            , option [ value "characters", selected (formData.patternType == "characters") ] [ text "String" ]
                            ]
                        ]
                    , label []
                        [ text "Matching"
                        , input [ placeholder "'a', 'b', 'c'", value formData.matching, onInput (ChangeForm ChangeMatching) ] []
                        ]
                    ]
                , div []
                    [ label []
                        [ text "Name"
                        , input [ placeholder "Loglevel oneof", value formData.name, onInput (ChangeForm ChangeName) ] []
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
