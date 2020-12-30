module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import DecEnc
import Html exposing (Html, a, button, div, h2, input, label, li, nav, option, select, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, map3, string)
import Json.Encode as Encode
import Page.LogfileParserApplication as LogfileParserApplication
import Page.LogfileParserCreation as LogfileParserCreation
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
    = RouteNotFound Session
    | ApplyLogfileParser LogfileParserApplication.Model
    | CreateParser ParserCreation.Model
    | CreateLogfileParser LogfileParserCreation.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Parser.parse routeParser url) (RouteNotFound { key = key })



-- UPDATE


type Msg
    = GotApplyLogfileParserMsg LogfileParserApplication.Msg
    | GotCreateParserMsg ParserCreation.Msg
    | GotCreateLogfileParserMsg LogfileParserCreation.Msg
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotApplyLogfileParserMsg applyMsg, ApplyLogfileParser applyModel ) ->
            let
                ( retModel, retCmd ) =
                    LogfileParserApplication.update applyMsg applyModel
            in
            ( ApplyLogfileParser retModel, Cmd.map GotApplyLogfileParserMsg retCmd )

        ( GotCreateParserMsg parserMsg, CreateParser parserModel ) ->
            let
                ( retModel, retCmd ) =
                    ParserCreation.update parserMsg parserModel
            in
            ( CreateParser retModel, Cmd.map GotCreateParserMsg retCmd )

        ( GotCreateLogfileParserMsg parserMsg, CreateLogfileParser parserModel ) ->
            let
                ( retModel, retCmd ) =
                    LogfileParserCreation.update parserMsg parserModel
            in
            ( CreateLogfileParser retModel, Cmd.map GotCreateLogfileParserMsg retCmd )

        ( ClickedLink urlRequest, CreateParser (ParserCreation.CreateParser session _) ) ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl session.key (Url.toString url) )

        ( ClickedLink urlRequest, CreateLogfileParser (LogfileParserCreation.CreateLogfileParser session _) ) ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl session.key (Url.toString url) )

        ( ClickedLink urlRequest, ApplyLogfileParser (LogfileParserApplication.ApplyLogfileParser session _) ) ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl session.key (Url.toString url) )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Parser.parse routeParser url) model

        -- Handle cases that should never happen
        ( GotApplyLogfileParserMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotCreateParserMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotCreateLogfileParserMsg _, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink _, _ ) ->
            ( model, Cmd.none )



-- ROUTING


type Route
    = ApplyParserRoute
    | CreateParserRoute
    | ParseLogfileRoute


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map ApplyParserRoute (s "apply-logfile")
        , Parser.map CreateParserRoute Parser.top
        , Parser.map ParseLogfileRoute (s "parse-logfile")
        ]


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( RouteNotFound session, Cmd.none )

        Just ApplyParserRoute ->
            case model of
                ApplyLogfileParser _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( retModel, retCmd ) =
                            LogfileParserApplication.init session
                    in
                    ( ApplyLogfileParser retModel, Cmd.map GotApplyLogfileParserMsg retCmd )

        Just CreateParserRoute ->
            case model of
                CreateParser _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( retModel, retCmd ) =
                            ParserCreation.init session
                    in
                    ( CreateParser retModel, Cmd.map GotCreateParserMsg retCmd )

        Just ParseLogfileRoute ->
            case model of
                CreateLogfileParser _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( retModel, retCmd ) =
                            LogfileParserCreation.init session
                    in
                    ( CreateLogfileParser retModel, Cmd.map GotCreateLogfileParserMsg retCmd )


toSession : Model -> Session
toSession model =
    case model of
        RouteNotFound session ->
            session

        ApplyLogfileParser (LogfileParserApplication.ApplyLogfileParser session _) ->
            session

        CreateParser (ParserCreation.CreateParser session _) ->
            session

        CreateLogfileParser (LogfileParserCreation.CreateLogfileParser session _) ->
            session



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        { title, centerContent } =
            viewCenter model
    in
    { title = title
    , body = [ viewPageLayout centerContent ]
    }


viewPageLayout : Html Msg -> Html Msg
viewPageLayout center =
    div [ class "page" ]
        [ div [ class "page__navbar" ] [ viewNavbar ]
        , div [ class "page__sidebar-left" ] []
        , div [ class "page__center" ] [ center ]
        , div [ class "page_sidebar-right" ] []
        ]


viewNavbar : Html Msg
viewNavbar =
    nav [ class "navbar", class "navbar--desktop" ]
        (List.map (\( url, desc ) -> a [ href url, class "navbar__link" ] [ text desc ])
            [ ( "https://wikipedia.org", "Wikipedia" )
            , ( "http://localhost:8081/", "Create Parser" )
            , ( "http://localhost:8081/parse-logfile", "Create Logfile Parser" )
            , ( "http://localhost:8081/apply-logfile", "Apply Parser" )
            ]
        )


viewCenter : Model -> { title : String, centerContent : Html Msg }
viewCenter model =
    case model of
        RouteNotFound _ ->
            { title = "Size not found"
            , centerContent = div [] [ text "No size could be found for the url you requested." ]
            }

        ApplyLogfileParser (LogfileParserApplication.ApplyLogfileParser session applyModel) ->
            { title = "Apply logfile parser"
            , centerContent = Html.map GotApplyLogfileParserMsg (LogfileParserApplication.view applyModel)
            }

        CreateParser (ParserCreation.CreateParser session parserModel) ->
            { title = "It works?!"
            , centerContent = Html.map GotCreateParserMsg (ParserCreation.view parserModel)
            }

        CreateLogfileParser (LogfileParserCreation.CreateLogfileParser session parserModel) ->
            { title = "Logfile Parsing"
            , centerContent = Html.map GotCreateLogfileParserMsg (LogfileParserCreation.view parserModel)
            }
