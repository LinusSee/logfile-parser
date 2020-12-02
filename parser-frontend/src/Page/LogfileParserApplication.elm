module Page.LogfileParserApplication exposing (..)

import DecEnc
import Html exposing (Html, a, button, div, h2, label, option, p, select, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Session exposing (Session)



-- MODEL


type Model
    = ApplyLogfileParser Session ApplyLogfileParserModel


type alias ApplyLogfileParserModel =
    { requestState : HttpRequestState
    , existingParsers : List String
    , chosenParser : String
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
        { requestState = Success
        , existingParsers = []
        , chosenParser = ""
        , stringToParse = ""
        , parsingResult = []
        }
    , Http.get
        { url = "http://localhost:8080/api/parsers/logfile"
        , expect = Http.expectJson GotLogfileParserNames DecEnc.logfileParserNames
        }
    )



-- UPDATE


type Msg
    = NoMsgYet
    | SelectLogfileParser String
    | ChangeStringToParse String
    | ApplyParser
    | GotLogfileParserNames (Result Http.Error (List String))
    | GotParsingResult (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (ApplyLogfileParser session model) =
    case msg of
        NoMsgYet ->
            ( ApplyLogfileParser session model, Cmd.none )

        SelectLogfileParser selectedName ->
            ( ApplyLogfileParser session { model | chosenParser = selectedName }
            , Cmd.none
            )

        ChangeStringToParse newString ->
            ( ApplyLogfileParser session { model | stringToParse = newString }
            , Cmd.none
            )

        ApplyParser ->
            ( ApplyLogfileParser session model
            , Http.get
                { url = "http://localhost:8080/api/parsers/logfile/apply1/StandardParser?target=2000-10-1111:30"
                , expect = Http.expectJson GotParsingResult DecEnc.logfileParserApplicationDecoder
                }
            )

        GotLogfileParserNames response ->
            case response of
                Ok data ->
                    ( ApplyLogfileParser
                        session
                        { requestState = Success
                        , existingParsers = data
                        , chosenParser = model.chosenParser
                        , stringToParse = model.stringToParse
                        , parsingResult = model.parsingResult
                        }
                    , Cmd.none
                    )

                Err error ->
                    Debug.log (Debug.toString error) ( ApplyLogfileParser session model, Cmd.none )

        GotParsingResult response ->
            case response of
                Ok data ->
                    ( ApplyLogfileParser
                        session
                        { requestState = Success
                        , existingParsers = model.existingParsers
                        , chosenParser = model.chosenParser
                        , stringToParse = model.stringToParse
                        , parsingResult = data
                        }
                    , Cmd.none
                    )

                Err error ->
                    Debug.log (Debug.toString error) ( ApplyLogfileParser session model, Cmd.none )



-- VIEW


view : ApplyLogfileParserModel -> Html Msg
view model =
    case model.requestState of
        Failure ->
            div [] [ text "Failed to load data" ]

        Loading ->
            div [] [ text "Loading..." ]

        Success ->
            div []
                [ h2 [] [ text "Apply an existing parser to a logfile" ]
                , div []
                    [ viewLogfileParserDropdown model.chosenParser model.existingParsers
                    , text model.chosenParser
                    , viewParserApplication model.stringToParse
                    ]
                , p [] [ text ("ParsingResult: (" ++ String.join ", " model.parsingResult ++ ")") ]
                , a [ href "http://localhost:8081/parse-logfile" ] [ text "Parse logfile" ]
                , a [ href "http://localhost:8081/" ] [ text "Create parser" ]
                ]


viewLogfileParserDropdown : String -> List String -> Html Msg
viewLogfileParserDropdown selection parserNames =
    div []
        [ label [] [ text "Logfile Parser" ]
        , select
            [ value selection, onInput SelectLogfileParser ]
            (List.map (\name -> option [ value name, selected (selection == name) ] [ text name ]) parserNames)
        ]


viewParserApplication : String -> Html Msg
viewParserApplication stringToParse =
    div []
        [ label []
            [ textarea [ placeholder "String to parse", value stringToParse, onInput ChangeStringToParse ] []
            ]
        , button [ onClick ApplyParser ] [ text "Apply" ]
        ]
