module Page.LogfileParsing exposing (..)

import DecEnc
import Html exposing (Html, div, text)
import Http
import Session exposing (Session)



-- MODEL


type Model
    = CreateLogfileParser Session CreateLogfileParserModel


type alias CreateLogfileParserModel =
    { requestState : HttpRequestState
    , existingParsers : List DecEnc.ElementaryParser
    , chosenParsers : List DecEnc.ElementaryParser
    , selectedParser : String
    }


type HttpRequestState
    = Failure
    | Loading
    | Success


init : Session -> ( Model, Cmd Msg )
init session =
    ( CreateLogfileParser
        session
        { requestState = Loading
        , existingParsers = []
        , chosenParsers = []
        , selectedParser = ""
        }
    , Http.get
        { url = "http://localhost:8080/api/parsers/building-blocks/complex"
        , expect = Http.expectJson GotElementaryParsers DecEnc.parsersDataDecoder
        }
    )



-- UPDATE


type Msg
    = NothingYet
    | GotElementaryParsers (Result Http.Error (List DecEnc.ElementaryParser))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (CreateLogfileParser session model) =
    case msg of
        GotElementaryParsers response ->
            case response of
                Ok data ->
                    case model.requestState of
                        Failure ->
                            ( CreateLogfileParser session { model | requestState = Failure }, Cmd.none )

                        Loading ->
                            ( CreateLogfileParser session
                                { requestState = Success
                                , existingParsers = []
                                , chosenParsers = []
                                , selectedParser = ""
                                }
                            , Cmd.none
                            )

                        Success ->
                            ( CreateLogfileParser session { model | existingParsers = data }, Cmd.none )

                Err error ->
                    Debug.log (Debug.toString error) ( CreateLogfileParser session { model | requestState = Failure }, Cmd.none )

        NothingYet ->
            ( CreateLogfileParser session model, Cmd.none )



-- VIEW


view : CreateLogfileParserModel -> Html Msg
view model =
    case model.requestState of
        Failure ->
            div [] [ text "Failed to load data" ]

        Loading ->
            div [] [ text "Loading..." ]

        Success ->
            div []
                [ text "Success loading 'ParseLogfile'!"
                ]
