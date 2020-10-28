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
import Page.ParserCreation as ParserCreation
import Session exposing (Session)
import Url
import Url.Parser as Parser exposing (Parser, oneOf, s)



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



-- MODEL


type Model
    = NotFound Session
    | CreateParser ParserCreation.Model
    | ParseLogfile Session


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Parser.parse routeParser url) (NotFound { key = key })



-- UPDATE


type Msg
    = GotDummyData (Result Http.Error DecEnc.SampleData)
    | GotElementaryParsers (Result Http.Error (List DecEnc.ElementaryParser))
    | PostedParser (Result Http.Error ())
    | ChangeForm ParserCreation.FormChanged String
    | Reset
    | Submit DecEnc.ParserFormData
    | GotStuff ParserCreation.Msg
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotDummyData result, CreateParser (ParserCreation.CreateParser session parserModel) ) ->
            case result of
                Ok data ->
                    case parserModel.requestState of
                        ParserCreation.Failure ->
                            ( model, Cmd.none )

                        ParserCreation.Loading ->
                            ( CreateParser
                                (ParserCreation.CreateParser session
                                    { parserModel
                                        | requestState =
                                            ParserCreation.Success
                                                { patternType = "oneOf"
                                                , matching = ""
                                                , name = ""
                                                }
                                                data
                                                []
                                    }
                                )
                            , Cmd.none
                            )

                        ParserCreation.Success formData _ existingParsers ->
                            ( CreateParser
                                (ParserCreation.CreateParser
                                    session
                                    { parserModel
                                        | requestState =
                                            ParserCreation.Success
                                                formData
                                                data
                                                existingParsers
                                    }
                                )
                            , Cmd.none
                            )

                Err error ->
                    Debug.log (Debug.toString error) ( CreateParser (ParserCreation.CreateParser session { parserModel | requestState = ParserCreation.Failure }), Cmd.none )

        ( GotElementaryParsers result, CreateParser (ParserCreation.CreateParser session parserModel) ) ->
            case result of
                Ok data ->
                    case parserModel.requestState of
                        ParserCreation.Failure ->
                            ( CreateParser (ParserCreation.CreateParser session { parserModel | requestState = ParserCreation.Failure }), Cmd.none )

                        ParserCreation.Loading ->
                            ( CreateParser
                                (ParserCreation.CreateParser session
                                    { parserModel
                                        | requestState =
                                            ParserCreation.Success
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
                                )
                            , Cmd.none
                            )

                        ParserCreation.Success formData loadedData _ ->
                            ( CreateParser
                                (ParserCreation.CreateParser session
                                    { parserModel
                                        | requestState =
                                            ParserCreation.Success
                                                formData
                                                loadedData
                                                data
                                    }
                                )
                            , Cmd.none
                            )

                Err error ->
                    Debug.log (Debug.toString error) ( CreateParser (ParserCreation.CreateParser session { parserModel | requestState = ParserCreation.Failure }), Cmd.none )

        ( PostedParser result, CreateParser (ParserCreation.CreateParser session parserModel) ) ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err error ->
                    Debug.log (Debug.toString error) ( CreateParser (ParserCreation.CreateParser session { parserModel | requestState = ParserCreation.Failure }), Cmd.none )

        ( ChangeForm field newContent, CreateParser (ParserCreation.CreateParser session parserModel) ) ->
            case parserModel.requestState of
                ParserCreation.Failure ->
                    ( model, Cmd.none )

                ParserCreation.Loading ->
                    ( CreateParser (ParserCreation.CreateParser session { parserModel | requestState = ParserCreation.Failure }), Cmd.none )

                ParserCreation.Success formData loadedData existingParsers ->
                    case field of
                        ParserCreation.ChangePatternType ->
                            ( CreateParser
                                (ParserCreation.CreateParser session
                                    { parserModel
                                        | requestState =
                                            ParserCreation.Success { formData | patternType = newContent }
                                                loadedData
                                                existingParsers
                                    }
                                )
                            , Cmd.none
                            )

                        ParserCreation.ChangeMatching ->
                            ( CreateParser
                                (ParserCreation.CreateParser session
                                    { parserModel
                                        | requestState =
                                            ParserCreation.Success { formData | matching = newContent }
                                                loadedData
                                                existingParsers
                                    }
                                )
                            , Cmd.none
                            )

                        ParserCreation.ChangeName ->
                            ( CreateParser
                                (ParserCreation.CreateParser session
                                    { parserModel
                                        | requestState =
                                            ParserCreation.Success { formData | name = newContent }
                                                loadedData
                                                existingParsers
                                    }
                                )
                            , Cmd.none
                            )

        ( Reset, CreateParser (ParserCreation.CreateParser session parserModel) ) ->
            ( CreateParser
                (ParserCreation.CreateParser session
                    { parserModel
                        | requestState =
                            ParserCreation.Success
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
                )
            , Cmd.none
            )

        ( Submit formData, _ ) ->
            ( model
            , postParser formData
            )

        ( ClickedLink urlRequest, CreateParser (ParserCreation.CreateParser session parserModel) ) ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl session.key (Url.toString url) )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Parser.parse routeParser url) model

        ( _, _ ) ->
            -- Disregard invalid combinations
            -- ( model, Cmd.none )
            Debug.todo "Should be the case for testing"



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

                CreateParser (ParserCreation.CreateParser session parserModel) ->
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
                    ( CreateParser (ParserCreation.CreateParser session { requestState = ParserCreation.Loading })
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

                CreateParser _ ->
                    ( model
                    , Cmd.none
                    )

                ParseLogfile session ->
                    ( CreateParser (ParserCreation.CreateParser session { requestState = ParserCreation.Loading })
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

                CreateParser (ParserCreation.CreateParser session _) ->
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

        CreateParser (ParserCreation.CreateParser session parserModel) ->
            -- { title = "Hello World", body = [ div [] [ text "Empty stuff" ] ] }
            { title = "It works?!", body = [ Html.map GotStuff (viewCreateParser parserModel) ] }

        ParseLogfile _ ->
            viewParseLogfile



-- viewCreateParser : ParserCreation.CreateParserModel -> Browser.Document Msg
-- viewCreateParser model =
--     case model.requestState of
--         ParserCreation.Failure ->
--             { title = "Hello World", body = [ div [] [ text "Failed to load data" ] ] }
--
--         ParserCreation.Loading ->
--             { title = "Hello World", body = [ div [] [ text "Loading..." ] ] }
--
--         ParserCreation.Success formData loadedData existingParsers ->
--             { title = "Hello World"
--             , body =
--                 [ div []
--                     [ h2 [] [ text "Create specialized parsers" ]
--                     , div []
--                         [ label []
--                             [ text "Type"
--                             , select [ value formData.patternType, onInput (ChangeForm ChangePatternType) ]
--                                 [ option [ value "oneOf", selected (formData.patternType == "oneOf") ] [ text "One Of" ]
--                                 , option [ value "date", selected (formData.patternType == "date") ] [ text "Date" ]
--                                 , option [ value "time", selected (formData.patternType == "time") ] [ text "Time" ]
--                                 , option [ value "characters", selected (formData.patternType == "characters") ] [ text "String" ]
--                                 ]
--                             ]
--                         , label []
--                             [ text "Matching"
--                             , input [ placeholder "'a', 'b', 'c'", value formData.matching, onInput (ChangeForm ChangeMatching) ] []
--                             ]
--                         ]
--                     , div []
--                         [ label []
--                             [ text "Name"
--                             , input [ placeholder "Loglevel oneof", value formData.name, onInput (ChangeForm ChangeName) ] []
--                             ]
--                         ]
--                     , div []
--                         [ button [ onClick Reset ] [ text "Reset" ]
--                         , button [ onClick (Submit formData) ] [ text "Submit" ]
--                         ]
--                     , text ("Loaded this string: " ++ loadedData.val2)
--                     , ul [] (List.map viewParser existingParsers)
--                     ]
--                 , a [ href "https://wikipedia.org" ] [ text "External link" ]
--                 , a [ href "http://localhost:8081/parse-logfile" ] [ text "Internal link" ]
--                 ]
--             }


viewCreateParser : ParserCreation.CreateParserModel -> Html ParserCreation.Msg
viewCreateParser model =
    case model.requestState of
        ParserCreation.Failure ->
            div [] [ text "Failed to load data" ]

        ParserCreation.Loading ->
            div [] [ text "Loading..." ]

        ParserCreation.Success formData loadedData existingParsers ->
            div []
                [ div []
                    [ h2 [] [ text "Create specialized parsers" ]
                    , div []
                        [ label []
                            [ text "Type"
                            , select [ value formData.patternType, onInput (ParserCreation.ChangeForm ParserCreation.ChangePatternType) ]
                                [ option [ value "oneOf", selected (formData.patternType == "oneOf") ] [ text "One Of" ]
                                , option [ value "date", selected (formData.patternType == "date") ] [ text "Date" ]
                                , option [ value "time", selected (formData.patternType == "time") ] [ text "Time" ]
                                , option [ value "characters", selected (formData.patternType == "characters") ] [ text "String" ]
                                ]
                            ]
                        , label []
                            [ text "Matching"
                            , input [ placeholder "'a', 'b', 'c'", value formData.matching, onInput (ParserCreation.ChangeForm ParserCreation.ChangeMatching) ] []
                            ]
                        ]
                    , div []
                        [ label []
                            [ text "Name"
                            , input [ placeholder "Loglevel oneof", value formData.name, onInput (ParserCreation.ChangeForm ParserCreation.ChangeName) ] []
                            ]
                        ]
                    , div []
                        [ button [ onClick ParserCreation.Reset ] [ text "Reset" ]
                        , button [ onClick (ParserCreation.Submit formData) ] [ text "Submit" ]
                        ]
                    , text ("Loaded this string: " ++ loadedData.val2)
                    , ul [] (List.map viewParser existingParsers)
                    ]
                , a [ href "https://wikipedia.org" ] [ text "External link" ]
                , a [ href "http://localhost:8081/parse-logfile" ] [ text "Internal link" ]
                ]


viewParseLogfile : Browser.Document Msg
viewParseLogfile =
    { title = "Parse Logfile"
    , body =
        [ text "Success loading 'ParseLogfile'!"
        ]
    }


viewParser : DecEnc.ElementaryParser -> Html ParserCreation.Msg
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
