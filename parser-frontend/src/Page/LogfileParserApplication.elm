module Page.LogfileParserApplication exposing (..)

import DecEnc
import Html exposing (Html, a, article, button, div, h2, label, option, p, select, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Session exposing (Session)
import Url.Builder as UrlBuilder



-- MODEL


type Model
    = ApplyLogfileParser Session ApplyLogfileParserModel


type alias ApplyLogfileParserModel =
    { requestState : HttpRequestState
    , existingParsers : List String
    , chosenParser : String
    , stringToParse : String
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
    | GotParsingResult (Result Http.Error (List (List ( String, String ))))


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
                { url =
                    UrlBuilder.crossOrigin
                        "http://localhost:8080/api/parsers/logfile/apply"
                        [ model.chosenParser ]
                        [ UrlBuilder.string "target" model.stringToParse ]
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
                        , chosenParser =
                            case List.head data of
                                Just firstName ->
                                    firstName

                                Nothing ->
                                    model.chosenParser
                        , stringToParse = model.stringToParse
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
                        , stringToParse = model.stringToParse
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
                    ++ viewParserApplication model.stringToParse
                    ++ viewParsingResult model
                )


viewLogfileParserDropdown : String -> List String -> Html Msg
viewLogfileParserDropdown selection parserNames =
    div [ class "input-group", class "input-group--centered-content" ]
        [ label [ for "parserSelect" ] [ text "Parser" ]
        , select
            [ id "parserSelect", value selection, onInput SelectLogfileParser ]
            (List.map (\name -> option [ value name, selected (selection == name) ] [ text name ]) parserNames)
        ]


viewParserApplication : String -> List (Html Msg)
viewParserApplication stringToParse =
    [ div [ class "input-group", class "input-group--centered-content" ]
        [ label [] [ text "Target string" ]
        , textarea [ placeholder "String to parse", value stringToParse, onInput ChangeStringToParse ] []
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
