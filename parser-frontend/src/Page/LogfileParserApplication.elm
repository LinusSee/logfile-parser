module Page.LogfileParserApplication exposing (..)

import DecEnc
import Html exposing (Html, div, text)
import Session exposing (Session)



-- MODEL


type Model
    = ApplyLogfileParser Session ApplyLogfileParserModel


type alias ApplyLogfileParserModel =
    { requestState : HttpRequestState
    , existingParsers : List DecEnc.ElementaryParser
    , chosenParser : Maybe DecEnc.LogfileParser
    , stringToParse : String
    , parsingResult : List String
    }


type HttpRequestState
    = Failure
    | Loading
    | Success


init : Session -> ( Model, Cmd Msg )
init session =
    ( ApplyLogfileParser session
        { requestState = Loading
        , existingParsers = []
        , chosenParser = Nothing
        , stringToParse = ""
        , parsingResult = []
        }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoMsgYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (ApplyLogfileParser session model) =
    case msg of
        NoMsgYet ->
            ( ApplyLogfileParser session model, Cmd.none )



-- VIEW


view : ApplyLogfileParserModel -> Html Msg
view model =
    case model.requestState of
        Failure ->
            div [] [ text "Failed to load data" ]

        Loading ->
            div [] [ text "Loading..." ]

        Success ->
            div [] [ text "Success loading data!" ]
