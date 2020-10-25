module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, h2, input, label, li, option, select, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, map3, string)
import Json.Encode as Encode
import Url



-- MAIN


main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
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
    | Success ParserFormData SampleData (List ElementaryParser)


type alias ParserFormData =
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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( Loading
    , Cmd.batch
        [ Http.get
            { url = "http://localhost:8080/api/sample"
            , expect = Http.expectJson GotDummyData sampleDataDecoder
            }
        , Http.get
            { url = "http://localhost:8080/api/parsers/building-blocks/complex"
            , expect = Http.expectJson GotElementaryParsers parsersDataDecoder
            }
        ]
    )



-- UPDATE


type FormChanged
    = ChangePatternType
    | ChangeMatching
    | ChangeName


type Msg
    = GotDummyData (Result Http.Error SampleData)
    | GotElementaryParsers (Result Http.Error (List ElementaryParser))
    | PostedParser (Result Http.Error ())
    | ChangeForm FormChanged String
    | Reset
    | Submit ParserFormData
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDummyData result ->
            case result of
                Ok data ->
                    case model of
                        Failure ->
                            ( Failure, Cmd.none )

                        Loading ->
                            ( Success
                                { patternType = "oneOf"
                                , matching = ""
                                , name = ""
                                }
                                data
                                []
                            , Cmd.none
                            )

                        Success formData _ existingParsers ->
                            ( Success
                                formData
                                data
                                existingParsers
                            , Cmd.none
                            )

                Err error ->
                    Debug.log (Debug.toString error) ( Failure, Cmd.none )

        GotElementaryParsers result ->
            case result of
                Ok data ->
                    case model of
                        Failure ->
                            ( Failure, Cmd.none )

                        Loading ->
                            ( Success
                                { patternType = "oneOf"
                                , matching = ""
                                , name = ""
                                }
                                { val1 = 0
                                , val2 = ""
                                , val3 = ""
                                }
                                data
                            , Cmd.none
                            )

                        Success formData loadedData _ ->
                            ( Success
                                formData
                                loadedData
                                data
                            , Cmd.none
                            )

                Err error ->
                    Debug.log (Debug.toString error) ( Failure, Cmd.none )

        PostedParser result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

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

        Submit formData ->
            ( model
            , postParser formData
            )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    Debug.todo "Internal link clicked"

        ChangedUrl _ ->
            Debug.todo "Url has been changed"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        Failure ->
            { title = "Hello World", body = [ div [] [ text "Failed to load data" ] ] }

        Loading ->
            { title = "Hello World", body = [ div [] [ text "Loading..." ] ] }

        Success formData loadedData existingParsers ->
            { title = "Hello World"
            , body =
                [ div []
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
                        , button [ onClick (Submit formData) ] [ text "Submit" ]
                        ]
                    , text ("Loaded this string: " ++ loadedData.val2)
                    , ul [] (List.map viewParser existingParsers)
                    ]
                , a [ href "https://wikipedia.org" ] [ text "External link" ]
                , a [ href "http://localhost:8081/otherPage" ] [ text "Internal link" ]
                ]
            }


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


postParser : ParserFormData -> Cmd Msg
postParser formData =
    Http.post
        { url = "http://localhost:8080/api/parsers/building-blocks/complex"
        , body = Http.jsonBody (parserEncoder formData)
        , expect = Http.expectWhatever PostedParser
        }


sampleDataDecoder : Decoder SampleData
sampleDataDecoder =
    map3 SampleData
        (field "dummy1" int)
        (field "dummy2" string)
        (field "dummy3" string)



-- Maybe TEMP


toMatchingList : String -> List String
toMatchingList matching =
    [ "HARDCODED", "DATA" ]


parserEncoder : ParserFormData -> Encode.Value
parserEncoder formData =
    case formData.patternType of
        "oneOf" ->
            Encode.object
                [ ( "type", Encode.string "oneOf" )
                , ( "values", Encode.list Encode.string (toMatchingList formData.matching) )
                ]

        "time" ->
            Encode.object
                [ ( "type", Encode.string "time" )
                , ( "pattern", Encode.string formData.matching )
                ]

        "date" ->
            Encode.object
                [ ( "type", Encode.string "date" )
                , ( "pattern", Encode.string formData.matching )
                ]

        "characters" ->
            Encode.object
                [ ( "type", Encode.string "characters" )
                , ( "value", Encode.string formData.matching )
                ]

        -- TEMP: Need to find out how to solve this
        _ ->
            Encode.object
                [ ( "type", Encode.string "invalidType" )
                , ( "value", Encode.string "invalidValue" )
                ]


parsersDataDecoder : Decoder (List ElementaryParser)
parsersDataDecoder =
    Decode.list parserDataDecoder


parserDataDecoder : Decoder ElementaryParser
parserDataDecoder =
    field "type" string
        |> Decode.andThen parserDataDecoderHelp


parserDataDecoderHelp : String -> Decoder ElementaryParser
parserDataDecoderHelp typeName =
    case typeName of
        "oneOf" ->
            oneOfParserDecoder

        "time" ->
            timeParserDecoder

        "date" ->
            dateParserDecoder

        "characters" ->
            charactersParserDecoder

        _ ->
            Decode.fail <|
                "Trying to decode parser but found incorrect type."
                    ++ "Type was "
                    ++ typeName
                    ++ "but expected one of "
                    ++ "[ \"oneOf\", \"time\", \"date\", \"characters\" ]"


oneOfParserDecoder : Decoder ElementaryParser
oneOfParserDecoder =
    Decode.map OneOf (field "values" (Decode.list string))


timeParserDecoder : Decoder ElementaryParser
timeParserDecoder =
    Decode.map Time (field "pattern" string)


charactersParserDecoder : Decoder ElementaryParser
charactersParserDecoder =
    Decode.map Characters (field "value" string)


dateParserDecoder : Decoder ElementaryParser
dateParserDecoder =
    Decode.map Date (field "pattern" string)
