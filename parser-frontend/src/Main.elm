module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h2, input, label, li, option, select, text, ul)
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



-- DUMMY DATA


parsers : List ElementaryParser
parsers =
    [ OneOf [ "Hello", "Darkness", "..." ]
    , Date "yyyy-mm-dd"
    ]



-- MODEL


type Model
    = Failure
    | Loading
    | Success PatternFormData SampleData (List ElementaryParser)


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


type alias TimePattern =
    String


type alias DatePattern =
    String


type ElementaryParser
    = OneOf (List String)
    | Time TimePattern
    | Date DatePattern
    | Characters String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "http://localhost:8080/api/sample"
        , expect = Http.expectJson GotDummyData sampleDataDecoder
        }
    )



-- UPDATE


type FormChanged
    = ChangePatternType
    | ChangeMatching
    | ChangeName


type Msg
    = GotDummyData (Result Http.Error SampleData)
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
                        parsers
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

                Success formData loadedData existingParsers ->
                    case field of
                        ChangePatternType ->
                            ( Success { formData | patternType = newContent }
                                loadedData
                                existingParsers
                            , Cmd.none
                            )

                        ChangeMatching ->
                            ( Success { formData | matching = newContent }
                                loadedData
                                existingParsers
                            , Cmd.none
                            )

                        ChangeName ->
                            ( Success { formData | name = newContent }
                                loadedData
                                existingParsers
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
                []
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
                []
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

        Success formData loadedData existingParsers ->
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
                , ul [] (List.map viewParser existingParsers)
                ]


viewParser : ElementaryParser -> Html Msg
viewParser parser =
    case parser of
        OneOf xs ->
            li [] [ text ("[ " ++ String.join ", " xs ++ " ]") ]

        Time pattern ->
            li [] [ text pattern ]

        Date pattern ->
            li [] [ text pattern ]

        Characters s ->
            li [] [ text s ]



-- HTTP


sampleDataDecoder : Decoder SampleData
sampleDataDecoder =
    map3 SampleData
        (field "dummy1" int)
        (field "dummy2" string)
        (field "dummy3" string)
