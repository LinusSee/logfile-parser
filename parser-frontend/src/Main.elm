module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import DecEnc
import Html exposing (Html, a, button, div, h2, input, label, li, option, select, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, map3, string)
import Json.Encode as Encode
import Url
import Url.Parser as Parser exposing (Parser, oneOf, s)



-- MAIN


type alias Session =
    { key : Nav.Key }


main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = NotFound Session
    | CreateParser Session CreateParserModel
    | ParseLogfile Session


type alias CreateParserModel =
    { requestState : HttpRequestState
    }


type HttpRequestState
    = Failure
    | Loading
    | Success DecEnc.ParserFormData DecEnc.SampleData (List DecEnc.ElementaryParser)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Parser.parse routeParser url) (NotFound { key = key })



-- UPDATE


type FormChanged
    = ChangePatternType
    | ChangeMatching
    | ChangeName


type Msg
    = GotDummyData (Result Http.Error DecEnc.SampleData)
    | GotElementaryParsers (Result Http.Error (List DecEnc.ElementaryParser))
    | PostedParser (Result Http.Error ())
    | ChangeForm FormChanged String
    | Reset
    | Submit DecEnc.ParserFormData
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotDummyData result, CreateParser session parserModel ) ->
            case result of
                Ok data ->
                    case parserModel.requestState of
                        Failure ->
                            ( model, Cmd.none )

                        Loading ->
                            ( CreateParser session
                                { parserModel
                                    | requestState =
                                        Success
                                            { patternType = "oneOf"
                                            , matching = ""
                                            , name = ""
                                            }
                                            data
                                            []
                                }
                            , Cmd.none
                            )

                        Success formData _ existingParsers ->
                            ( CreateParser session
                                { parserModel
                                    | requestState =
                                        Success
                                            formData
                                            data
                                            existingParsers
                                }
                            , Cmd.none
                            )

                Err error ->
                    Debug.log (Debug.toString error) ( CreateParser session { parserModel | requestState = Failure }, Cmd.none )

        ( GotElementaryParsers result, CreateParser session parserModel ) ->
            case result of
                Ok data ->
                    case parserModel.requestState of
                        Failure ->
                            ( CreateParser session { parserModel | requestState = Failure }, Cmd.none )

                        Loading ->
                            ( CreateParser session
                                { parserModel
                                    | requestState =
                                        Success
                                            { patternType = "oneOf"
                                            , matching = ""
                                            , name = ""
                                            }
                                            { val1 = 0
                                            , val2 = ""
                                            , val3 = ""
                                            }
                                            data
                                }
                            , Cmd.none
                            )

                        Success formData loadedData _ ->
                            ( CreateParser session
                                { parserModel
                                    | requestState =
                                        Success
                                            formData
                                            loadedData
                                            data
                                }
                            , Cmd.none
                            )

                Err error ->
                    Debug.log (Debug.toString error) ( CreateParser session { parserModel | requestState = Failure }, Cmd.none )

        ( PostedParser result, CreateParser session parserModel ) ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err error ->
                    Debug.log (Debug.toString error) ( CreateParser session { parserModel | requestState = Failure }, Cmd.none )

        ( ChangeForm field newContent, CreateParser session parserModel ) ->
            case parserModel.requestState of
                Failure ->
                    ( model, Cmd.none )

                Loading ->
                    ( CreateParser session { parserModel | requestState = Failure }, Cmd.none )

                Success formData loadedData existingParsers ->
                    case field of
                        ChangePatternType ->
                            ( CreateParser session
                                { parserModel
                                    | requestState =
                                        Success { formData | patternType = newContent }
                                            loadedData
                                            existingParsers
                                }
                            , Cmd.none
                            )

                        ChangeMatching ->
                            ( CreateParser session
                                { parserModel
                                    | requestState =
                                        Success { formData | matching = newContent }
                                            loadedData
                                            existingParsers
                                }
                            , Cmd.none
                            )

                        ChangeName ->
                            ( CreateParser session
                                { parserModel
                                    | requestState =
                                        Success { formData | name = newContent }
                                            loadedData
                                            existingParsers
                                }
                            , Cmd.none
                            )

        ( Reset, CreateParser session parserModel ) ->
            ( CreateParser session
                { parserModel
                    | requestState =
                        Success
                            { patternType = "oneOf"
                            , matching = ""
                            , name = ""
                            }
                            { val1 = 1
                            , val2 = ""
                            , val3 = ""
                            }
                            []
                }
            , Cmd.none
            )

        ( Submit formData, _ ) ->
            ( model
            , postParser formData
            )

        ( ClickedLink urlRequest, CreateParser session parserModel ) ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl session.key (Url.toString url) )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Parser.parse routeParser url) model

        ( _, _ ) ->
            -- Disregard invalid combinations
            ( model, Cmd.none )



-- ROUTING


type Route
    = CreateParserRoute
    | ParseLogfileRoute


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map CreateParserRoute Parser.top
        , Parser.map ParseLogfileRoute (s "parse-logfile")
        ]


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            case model of
                NotFound session ->
                    ( NotFound session
                    , Cmd.none
                    )

                CreateParser session parserModel ->
                    ( NotFound session
                    , Cmd.none
                    )

                ParseLogfile session ->
                    ( NotFound session
                    , Cmd.none
                    )

        Just CreateParserRoute ->
            case model of
                NotFound session ->
                    ( CreateParser session { requestState = Loading }
                    , Cmd.batch
                        [ Http.get
                            { url = "http://localhost:8080/api/sample"
                            , expect = Http.expectJson GotDummyData DecEnc.sampleDataDecoder
                            }
                        , Http.get
                            { url = "http://localhost:8080/api/parsers/building-blocks/complex"
                            , expect = Http.expectJson GotElementaryParsers DecEnc.parsersDataDecoder
                            }
                        ]
                    )

                CreateParser _ _ ->
                    ( model
                    , Cmd.none
                    )

                ParseLogfile session ->
                    ( CreateParser session { requestState = Loading }
                    , Cmd.batch
                        [ Http.get
                            { url = "http://localhost:8080/api/sample"
                            , expect = Http.expectJson GotDummyData DecEnc.sampleDataDecoder
                            }
                        , Http.get
                            { url = "http://localhost:8080/api/parsers/building-blocks/complex"
                            , expect = Http.expectJson GotElementaryParsers DecEnc.parsersDataDecoder
                            }
                        ]
                    )

        Just ParseLogfileRoute ->
            case model of
                NotFound session ->
                    ( ParseLogfile session
                    , Cmd.none
                    )

                CreateParser session _ ->
                    ( ParseLogfile session
                    , Cmd.none
                    )

                ParseLogfile session ->
                    ( ParseLogfile session
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        NotFound _ ->
            Debug.todo "not found"

        CreateParser session parserModel ->
            viewCreateParser parserModel

        ParseLogfile _ ->
            viewParseLogfile


viewCreateParser : CreateParserModel -> Browser.Document Msg
viewCreateParser model =
    case model.requestState of
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
                , a [ href "http://localhost:8081/parse-logfile" ] [ text "Internal link" ]
                ]
            }


viewParseLogfile : Browser.Document Msg
viewParseLogfile =
    { title = "Parse Logfile"
    , body =
        [ text "Success loading 'ParseLogfile'!"
        ]
    }


viewParser : DecEnc.ElementaryParser -> Html Msg
viewParser parser =
    case parser of
        DecEnc.OneOf xs ->
            li [] [ text ("[ " ++ String.join ", " xs ++ " ]") ]

        DecEnc.Time pattern ->
            li [] [ text pattern ]

        DecEnc.Date pattern ->
            li [] [ text pattern ]

        DecEnc.Characters s ->
            li [] [ text s ]



-- HTTP


postParser : DecEnc.ParserFormData -> Cmd Msg
postParser formData =
    Http.post
        { url = "http://localhost:8080/api/parsers/building-blocks/complex"
        , body = Http.jsonBody (DecEnc.parserEncoder formData)
        , expect = Http.expectWhatever PostedParser
        }
