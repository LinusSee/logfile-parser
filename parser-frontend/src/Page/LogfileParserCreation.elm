module Page.LogfileParserCreation exposing (..)

import DecEnc
import Html exposing (Html, a, article, button, div, h2, input, label, li, option, p, select, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Session exposing (Session)



-- MODEL


type Model
    = CreateLogfileParser Session CreateLogfileParserModel


type alias CreateLogfileParserModel =
    { requestState : HttpRequestState
    , existingParsers : List DecEnc.ElementaryParser
    , chosenParsers : List ( String, DecEnc.ElementaryParser )
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
    = GotElementaryParsers (Result Http.Error (List DecEnc.ElementaryParser))
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
                                        Just (DecEnc.OneOf name _) ->
                                            name

                                        Just (DecEnc.Time name _) ->
                                            name

                                        Just (DecEnc.Date name _) ->
                                            name

                                        Just (DecEnc.Characters name _) ->
                                            name

                                        Just (DecEnc.MatchUntilIncluded name _) ->
                                            name

                                        Just (DecEnc.MatchUntilExcluded name _) ->
                                            name

                                        Just (DecEnc.MatchFor name _) ->
                                            name

                                        Just (DecEnc.MatchUntilEnd name) ->
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


chooseParserByName : String -> List DecEnc.ElementaryParser -> Maybe DecEnc.ElementaryParser
chooseParserByName targetName parsers =
    let
        matchesName parser =
            case parser of
                DecEnc.OneOf name _ ->
                    targetName == name

                DecEnc.Time name _ ->
                    targetName == name

                DecEnc.Date name _ ->
                    targetName == name

                DecEnc.Characters name _ ->
                    targetName == name

                DecEnc.MatchUntilIncluded name _ ->
                    targetName == name

                DecEnc.MatchUntilExcluded name _ ->
                    targetName == name

                DecEnc.MatchFor name _ ->
                    targetName == name

                DecEnc.MatchUntilEnd name ->
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


viewParserSelection : String -> List DecEnc.ElementaryParser -> Html Msg
viewParserSelection selection parsers =
    div [ class "input-group", class "input-group--centered-content" ]
        [ label [] [ text "Parser" ]
        , select [ value selection, onInput SelectParserToAdd ] (List.map (parserToOption selection) parsers)
        , button [ onClick AddSelectedParser, class "standard-button" ] [ text "Apply" ]
        ]


viewParser : ( String, DecEnc.ElementaryParser ) -> Html Msg
viewParser ( name, parser ) =
    case parser of
        DecEnc.OneOf _ xs ->
            li [] [ text (name ++ ": [ " ++ String.join ", " xs ++ " ]") ]

        DecEnc.Time _ pattern ->
            li [] [ text (name ++ ": " ++ pattern) ]

        DecEnc.Date _ pattern ->
            li [] [ text (name ++ ": " ++ pattern) ]

        DecEnc.Characters _ s ->
            li [] [ text (name ++ ": " ++ s) ]

        DecEnc.MatchUntilIncluded _ s ->
            li [] [ text (name ++ ": " ++ s) ]

        DecEnc.MatchUntilExcluded _ s ->
            li [] [ text (name ++ ": " ++ s) ]

        DecEnc.MatchFor _ number ->
            li [] [ text (name ++ ": " ++ String.fromInt number) ]

        DecEnc.MatchUntilEnd _ ->
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


parserToOption : String -> DecEnc.ElementaryParser -> Html Msg
parserToOption selection parser =
    case parser of
        DecEnc.OneOf name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        DecEnc.Time name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        DecEnc.Date name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        DecEnc.Characters name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        DecEnc.MatchUntilIncluded name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        DecEnc.MatchUntilExcluded name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        DecEnc.MatchFor name _ ->
            option [ value name, selected (selection == name) ] [ text name ]

        DecEnc.MatchUntilEnd name ->
            option [ value name, selected (selection == name) ] [ text name ]



-- HTTP


postParser : DecEnc.LogfileParser -> Cmd Msg
postParser parser =
    Http.post
        { url = "http://localhost:8080/api/parsers/logfile"
        , body = Http.jsonBody (DecEnc.logfileParserEncoder parser)
        , expect = Http.expectWhatever PostedLogfileParser
        }


postApplyParser : String -> DecEnc.LogfileParser -> Cmd Msg
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
