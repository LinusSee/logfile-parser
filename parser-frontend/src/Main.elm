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
    | GotStuff ParserCreation.Msg
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotStuff parserMsg, CreateParser parserModel ) ->
            let
                ( retModel, retCmd ) =
                    ParserCreation.update parserMsg parserModel
            in
            ( CreateParser retModel, Cmd.map GotStuff retCmd )

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
            Debug.todo "Should never happen"



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
                    let
                        ( retModel, retCmd ) =
                            ParserCreation.init session
                    in
                    ( CreateParser retModel, Cmd.map GotStuff retCmd )

                -- ( CreateParser (ParserCreation.CreateParser session { requestState = ParserCreation.Loading })
                --   -- , Cmd.batch
                --   --     [ Http.get
                --   --         { url = "http://localhost:8080/api/sample"
                --   --         , expect = Http.expectJson GotDummyData DecEnc.sampleDataDecoder
                --   --         }
                --   --     , Http.get
                --   --         { url = "http://localhost:8080/api/parsers/building-blocks/complex"
                --   --         , expect = Http.expectJson GotElementaryParsers DecEnc.parsersDataDecoder
                --   --         }
                --   --     ]
                -- , Cmd.none
                -- )
                CreateParser _ ->
                    ( model
                    , Cmd.none
                    )

                ParseLogfile session ->
                    let
                        ( retModel, retCmd ) =
                            ParserCreation.init session
                    in
                    ( CreateParser retModel, Cmd.map GotStuff retCmd )

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
            { title = "It works?!", body = [ Html.map GotStuff (ParserCreation.view parserModel) ] }

        ParseLogfile _ ->
            viewParseLogfile


viewParseLogfile : Browser.Document Msg
viewParseLogfile =
    { title = "Parse Logfile"
    , body =
        [ text "Success loading 'ParseLogfile'!"
        ]
    }