module Page.LogfileParserCreation exposing (..)

import DecEnc
import Html exposing (Html, a, article, button, div, h2, input, label, li, option, p, select, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Models.Shared.ElementaryParser as ElementaryParser
import Models.Shared.LogfileParser as LogfileParser
import Session exposing (Session)



-- MODEL


type Model
    = CreateLogfileParser Session CreateLogfileParserModel


type alias CreateLogfileParserModel =
    { requestState : HttpRequestState
    , existingParsers : List ElementaryParser.ElementaryParser
    , chosenParsers : List ( String, ElementaryParser.ElementaryParser )
    , displayDropdown : Bool
    , parserName : String
    , selectedParser : String
    , nameForSelectedParser : String
    , stringToParse : String
    , parsingResult : List (List ( String, String ))
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
        , displayDropdown = True
        , parserName = ""
        , selectedParser = ""
        , nameForSelectedParser = ""
        , stringToParse = ""
        , parsingResult = []
        }
    , Http.get
        { url = "http://localhost:8080/api/parsers/building-blocks/complex"
        , expect = Http.expectJson GotElementaryParsers DecEnc.parsersDataDecoder
        }
    )



-- UPDATE


type Msg
    = GotElementaryParsers (Result Http.Error (List ElementaryParser.ElementaryParser))
    | ChangeParserName String
    | SelectParserToAdd String
    | ChangeNameForSelectedParser String
    | AddSelectedParser
    | ApplyParser
    | ChangeParsingContent String
    | PostedLogfileParser (Result Http.Error ())
    | GotParserApplicationResult (Result Http.Error (List (List ( String, String ))))
    | Submit


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
                                , existingParsers = data
                                , chosenParsers = []
                                , displayDropdown = True
                                , parserName = ""
                                , selectedParser =
                                    case List.head data of
                                        Just (ElementaryParser.OneOf name _) ->
                                            name

                                        Just (ElementaryParser.Time name _) ->
                                            name

                                        Just (ElementaryParser.Date name _) ->
                                            name

                                        Just (ElementaryParser.Characters name _) ->
                                            name

                                        Just (ElementaryParser.MatchUntilIncluded name _) ->
                                            name

                                        Just (ElementaryParser.MatchUntilExcluded name _) ->
                                            name

                                        Just (ElementaryParser.MatchFor name _) ->
                                            name

                                        Just (ElementaryParser.MatchUntilEnd name) ->
                                            name

                                        Nothing ->
                                            ""
                                , nameForSelectedParser = ""
                                , stringToParse = ""
                                , parsingResult = []
                                }
                            , Cmd.none
                            )

                        Success ->
                            ( CreateLogfileParser session { model | existingParsers = data }, Cmd.none )

                Err error ->
                    ( CreateLogfileParser session { model | requestState = Failure }, Cmd.none )

        ChangeParserName newName ->
            ( CreateLogfileParser
                session
                { model | parserName = newName }
            , Cmd.none
            )

        SelectParserToAdd selectedParser ->
            ( CreateLogfileParser
                session
                { model | selectedParser = selectedParser }
            , Cmd.none
            )

        ChangeNameForSelectedParser selectedName ->
            ( CreateLogfileParser
                session
                { model | nameForSelectedParser = selectedName }
            , Cmd.none
            )

        AddSelectedParser ->
            let
                maybeChosenParser =
                    chooseParserByName model.selectedParser model.existingParsers
            in
            case maybeChosenParser of
                Just chosenParser ->
                    ( CreateLogfileParser session
                        { requestState = model.requestState
                        , existingParsers = model.existingParsers
                        , chosenParsers = ( model.nameForSelectedParser, chosenParser ) :: model.chosenParsers
                        , displayDropdown = model.displayDropdown
                        , parserName = model.parserName
                        , selectedParser = model.selectedParser
                        , nameForSelectedParser = ""
                        , stringToParse = model.stringToParse
                        , parsingResult = model.parsingResult
                        }
                    , Cmd.none
                    )

                Nothing ->
                    ( CreateLogfileParser session model, Cmd.none )

        ChangeParsingContent newValue ->
            ( CreateLogfileParser session { model | stringToParse = newValue }, Cmd.none )

        ApplyParser ->
            ( CreateLogfileParser session model
            , postApplyParser model.stringToParse
                { name = model.parserName, parsers = model.chosenParsers }
            )

        PostedLogfileParser response ->
            case response of
                Ok _ ->
                    ( CreateLogfileParser session model, Cmd.none )

                Err error ->
                    ( CreateLogfileParser session { model | requestState = Failure }, Cmd.none )

        GotParserApplicationResult response ->
            case response of
                Ok data ->
                    ( CreateLogfileParser session { model | parsingResult = data }, Cmd.none )

                Err error ->
                    ( CreateLogfileParser session model, Cmd.none )

        Submit ->
            ( CreateLogfileParser session model
            , postParser { name = model.parserName, parsers = model.chosenParsers }
            )


chooseParserByName : String -> List ElementaryParser.ElementaryParser -> Maybe ElementaryParser.ElementaryParser
chooseParserByName targetName parsers =
    let
        matchesName parser =
            case parser of
                ElementaryParser.OneOf name _ ->
                    targetName == name

                ElementaryParser.Time name _ ->
                    targetName == name

                ElementaryParser.Date name _ ->
                    targetName == name

                ElementaryParser.Characters name _ ->
                    targetName == name

                ElementaryParser.MatchUntilIncluded name _ ->
                    targetName == name

                ElementaryParser.MatchUntilExcluded name _ ->
                    targetName == name

                ElementaryParser.MatchFor name _ ->
                    targetName == name

                ElementaryParser.MatchUntilEnd name ->
                    targetName == name
    in
    List.head (List.filter matchesName parsers)



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
                [ div [ class "article" ]
                    [ h2 [ class "header2--centered" ] [ text "Combine parsers to create a logfile parser" ]
                    , div [ class "input-group", class "input-group--centered-content" ]
                        [ label [ for "parserNameInput" ] [ text "Name" ]
                        , input [ id "parserNameInput", placeholder "Selected parser name", value model.parserName, onInput ChangeParserName ] [ text model.parserName ]
                        ]
                    , div [ class "input-group", class "input-group--centered-content" ]
                        [ label [ for "selectedParserName" ] [ text "ColumnName" ]
                        , input [ id "selectedParserName", placeholder "Name of the parsed value", value model.nameForSelectedParser, onInput ChangeNameForSelectedParser ]
                            [ text model.nameForSelectedParser ]
                        ]
                    , viewParserSelection model.selectedParser model.existingParsers
                    , div [ class "button-group", class "button-group--centered-content" ]
                        [ button [ onClick Submit, class "standard-button" ] [ text "Submit" ]
                        ]
                    , ul [ class "parser-list" ] (List.map viewParser model.chosenParsers)
                    ]
                , viewParserApplication model
                ]


viewParserSelection : String -> List ElementaryParser.ElementaryParser -> Html Msg
viewParserSelection selection parsers =
    div [ class "input-group", class "input-group--centered-content" ]
        [ label [] [ text "Parser" ]
        , select [ value selection, onInput SelectParserToAdd ] (List.map (parserToOption selection) parsers)
        , button [ onClick AddSelectedParser, class "standard-button" ] [ text "Apply" ]
        ]


viewParser : ( String, ElementaryParser.ElementaryParser ) -> Html Msg
viewParser ( name, parser ) =
    case parser of
        ElementaryParser.OneOf _ xs ->
            li [] [ text (name ++ ": [ " ++ String.join ", " xs ++ " ]") ]

        ElementaryParser.Time _ pattern ->
            li [] [ text (name ++ ": " ++ pattern) ]

        ElementaryParser.Date _ pattern ->
            li [] [ text (name ++ ": " ++ pattern) ]

        ElementaryParser.Characters _ s ->
            li [] [ text (name ++ ": " ++ s) ]

        ElementaryParser.MatchUntilIncluded _ s ->
            li [] [ text (name ++ ": " ++ s) ]

        ElementaryParser.MatchUntilExcluded _ s ->
            li [] [ text (name ++ ": " ++ s) ]

        ElementaryParser.MatchFor _ number ->
            li [] [ text (name ++ ": " ++ String.fromInt number) ]

        ElementaryParser.MatchUntilEnd _ ->
            li [] [ text name ]


viewParserApplication : CreateLogfileParserModel -> Html Msg
viewParserApplication model =
    article [ class "article" ]
        [ h2 [ class "header2--centered" ] [ text "Test current parser" ]
        , div [ class "input-group", class "input-group--centered-content" ]
            [ label [] [ text "Target" ]
            , textarea [ placeholder "String to parse", value model.stringToParse, onInput ChangeParsingContent ] []
            ]
        , div [ class "button-group", class "button-group--centered-content" ]
            [ button [ onClick ApplyParser, class "standard-button", class "standard-button--long" ] [ text "Apply" ]
            ]
        , div [ class "results" ]
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
        ]


parserToOption : String -> ElementaryParser.ElementaryParser -> Html Msg
parserToOption selection parser =
    case parser of
        ElementaryParser.OneOf name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        ElementaryParser.Time name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        ElementaryParser.Date name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        ElementaryParser.Characters name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        ElementaryParser.MatchUntilIncluded name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        ElementaryParser.MatchUntilExcluded name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        ElementaryParser.MatchFor name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        ElementaryParser.MatchUntilEnd name ->
            option [ value name, selected (selection == name) ] [ text name ]



-- HTTP


postParser : LogfileParser.LogfileParser -> Cmd Msg
postParser parser =
    Http.post
        { url = "http://localhost:8080/api/parsers/logfile"
        , body = Http.jsonBody (LogfileParser.logfileParserEncoder parser)
        , expect = Http.expectWhatever PostedLogfileParser
        }


postApplyParser : String -> LogfileParser.LogfileParser -> Cmd Msg
postApplyParser target parser =
    let
        data =
            { target = target, parser = parser }
    in
    Http.post
        { url = "http://localhost:8080/api/parsers/logfile/apply"
        , body = Http.jsonBody (DecEnc.logfileParserApplicationEncoder data)
        , expect = Http.expectJson GotParserApplicationResult DecEnc.logfileParserApplicationDecoder
        }
