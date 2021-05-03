module Page.LogfileParserApplication exposing (..)

import File exposing (File)
import File.Select as Select
import Html exposing (Html, a, article, button, div, h2, input, label, option, p, select, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Models.Shared.LogfileParser as LogfileParser
import Models.Shared.ParserApplication as ParserApplication
import Session exposing (Session)
import Url.Builder as UrlBuilder



-- MODEL


type Model
    = ApplyLogfileParser Session ApplyLogfileParserModel


type alias ApplyLogfileParserModel =
    { requestState : HttpRequestState
    , existingParsers : List LogfileParser.LogfileParserId
    , chosenParser : Maybe LogfileParser.LogfileParserId
    , selectedLogfile : Maybe File
    , parsingResult : List (List ( String, String ))
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
        , chosenParser = Nothing
        , selectedLogfile = Nothing
        , parsingResult = []
        }
    , Http.get
        { url = "http://localhost:8080/api/parsers/logfile"
        , expect = Http.expectJson GotLogfileParserIds LogfileParser.logfileParserIdsDecoder
        }
    )



-- UPDATE


type Msg
    = NoMsgYet
    | SelectLogfileParser String
    | LogfileRequested
    | LogfileSelected File
    | ApplyParser
    | GotLogfileParserIds (Result Http.Error (List LogfileParser.LogfileParserId))
    | GotParsingResult (Result Http.Error (List (List ( String, String ))))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (ApplyLogfileParser session model) =
    case msg of
        NoMsgYet ->
            ( ApplyLogfileParser session model, Cmd.none )

        SelectLogfileParser selectedId ->
            ( ApplyLogfileParser session { model | chosenParser = List.head (List.filter (\parserId -> parserId.parserId == selectedId) model.existingParsers) }
            , Cmd.none
            )

        LogfileRequested ->
            ( ApplyLogfileParser session model
            , Select.file [] LogfileSelected
            )

        LogfileSelected file ->
            ( ApplyLogfileParser session { model | selectedLogfile = Just file }
            , Cmd.none
            )

        ApplyParser ->
            case model.selectedLogfile of
                Just logfile ->
                    case model.chosenParser of
                        Just parserId ->
                            ( ApplyLogfileParser session model
                            , Http.post
                                { url = "http://localhost:8080/api/parsers/logfile/apply/file"
                                , body =
                                    Http.multipartBody
                                        [ Http.stringPart "name" parserId.name
                                        , Http.filePart "logfile" logfile
                                        ]
                                , expect = Http.expectJson GotParsingResult ParserApplication.logfileParserApplicationDecoder
                                }
                            )

                        Nothing ->
                            Debug.todo "No parser selected: Should not be possible and the user should be given a message"

                Nothing ->
                    Debug.todo "No file selected: Should not be possible and the user should be given a message"

        -- ( ApplyLogfileParser session model
        -- , Http.get
        --     { url =
        --         UrlBuilder.crossOrigin
        --             "http://localhost:8080/api/parsers/logfile/apply"
        --             [ model.chosenParser ]
        --             [ UrlBuilder.string "target" "TODO" ]
        --     , expect = Http.expectJson GotParsingResult ParserApplication.logfileParserApplicationDecoder
        --     }
        -- )
        GotLogfileParserIds response ->
            case response of
                Ok data ->
                    ( ApplyLogfileParser
                        session
                        { requestState = Success
                        , existingParsers = data
                        , chosenParser =
                            case List.head data of
                                Just firstId ->
                                    Just firstId

                                Nothing ->
                                    model.chosenParser
                        , selectedLogfile = model.selectedLogfile
                        , parsingResult = model.parsingResult
                        }
                    , Cmd.none
                    )

                Err error ->
                    ( ApplyLogfileParser session model, Cmd.none )

        GotParsingResult response ->
            case response of
                Ok data ->
                    ( ApplyLogfileParser
                        session
                        { requestState = Success
                        , existingParsers = model.existingParsers
                        , chosenParser = model.chosenParser
                        , selectedLogfile = model.selectedLogfile
                        , parsingResult = data
                        }
                    , Cmd.none
                    )

                Err error ->
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
            article [ class "article" ]
                ([ h2 [ class "header2--centered" ] [ text "Apply an existing parser to a logfile" ] ]
                    ++ [ viewLogfileParserDropdown model.chosenParser model.existingParsers ]
                    ++ viewParserApplication model.selectedLogfile
                    ++ viewParsingResult model
                )


viewLogfileParserDropdown : Maybe LogfileParser.LogfileParserId -> List LogfileParser.LogfileParserId -> Html Msg
viewLogfileParserDropdown maybeSelectedId parserIds =
    div [ class "input-group", class "input-group--centered-content" ]
        [ label [ for "parserSelect" ] [ text "Parser" ]
        , viewLogfileParserDropdownSelect maybeSelectedId parserIds
        ]


viewLogfileParserDropdownSelect : Maybe LogfileParser.LogfileParserId -> List LogfileParser.LogfileParserId -> Html Msg
viewLogfileParserDropdownSelect maybeSelectedId parserIds =
    case maybeSelectedId of
        Just selectedId ->
            select
                [ id "parserSelect", value selectedId.parserId, onInput SelectLogfileParser ]
                (List.map (\parserId -> option [ value parserId.parserId, selected (selectedId.parserId == parserId.parserId) ] [ text parserId.name ]) parserIds)

        Nothing ->
            select
                [ id "parserSelect", value "", onInput SelectLogfileParser ]
                (List.map (\parserId -> option [ value parserId.parserId, selected False ] [ text parserId.name ]) parserIds)


viewParserApplication : Maybe File -> List (Html Msg)
viewParserApplication maybeFile =
    [ div [ class "input-group", class "input-group--centered-content" ]
        [ label [] [ text "Target file" ]
        , button [ onClick LogfileRequested, class "standard-button standard-button--long" ] [ text "Upload file" ]
        , case maybeFile of
            Just file ->
                text ("Selected file: " ++ File.name file)

            Nothing ->
                text ""

        --, textarea [ placeholder "String to parse", value stringToParse, onInput ChangeStringToParse ] []
        ]
    , div [ class "button-group button-group--centered-content" ]
        [ button [ onClick ApplyParser, class "standard-button standard-button--long" ] [ text "Apply" ] ]
    ]


viewParsingResult : ApplyLogfileParserModel -> List (Html Msg)
viewParsingResult model =
    [ h2 [ class "header2--centered" ] [ text "Parsing result" ]
    , case List.head model.parsingResult of
        Nothing ->
            p [ class "text--centered" ] [ text "No result to display" ]

        Just firstElement ->
            let
                headers =
                    List.map Tuple.first firstElement

                content =
                    List.map (List.map Tuple.second) model.parsingResult
            in
            table []
                [ thead []
                    [ tr [] (List.map (\header -> th [] [ text header ]) headers)
                    ]
                , tbody []
                    (List.map (\row -> tr [] (List.map (\element -> td [] [ text element ]) row)) content)
                ]
    ]
