module Page.LogfileParserCreation exposing (..)

import Html exposing (Html, a, article, button, div, h2, input, label, li, option, p, select, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Models.Shared.ElementaryParser as ElementaryParser
import Models.Shared.LogfileParser as LogfileParser
import Models.Shared.ParserApplication as ParserApplication
import Session exposing (Session)
import Validate exposing (Validator, fromErrors, ifBlank, ifEmptyList, ifFalse, ifTrue, validate)



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
    , problems : List ValidationProblem
    }


type HttpRequestState
    = Failure
    | Loading
    | Success


type ValidationProblem
    = InvalidEntry ValidatedField String


type ValidatedField
    = ColumnName
    | SelectedParserName
    | LogfileParserName
    | ChosenParsers


type alias AddElementaryParserData =
    { columnName : String
    , parserName : String
    }


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
        , problems = []
        }
    , Http.get
        { url = "http://localhost:8080/api/parsers/building-blocks/complex"
        , expect = Http.expectJson GotElementaryParsers ElementaryParser.parsersDataDecoder
        }
    )



-- UPDATE


type Msg
    = GotElementaryParsers (Result Http.Error (List ElementaryParser.ElementaryParser))
    | ChangeParserName String
    | SelectParserToAdd String
    | ChangeNameForSelectedParser String
    | AddSelectedParser AddElementaryParserData
    | ApplyParser String LogfileParser.LogfileParser
    | ChangeParsingContent String
    | PostedLogfileParser (Result Http.Error ())
    | GotParserApplicationResult (Result Http.Error (List (List ( String, String ))))
    | Submit LogfileParser.LogfileParser


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
                                        Just (ElementaryParser.ElementaryParser name _) ->
                                            name

                                        Nothing ->
                                            ""
                                , nameForSelectedParser = ""
                                , stringToParse = ""
                                , parsingResult = []
                                , problems = []
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

        AddSelectedParser data ->
            case validateAddParserData model.existingParsers data of
                Ok validatedData ->
                    let
                        validated =
                            Validate.fromValid validatedData

                        maybeChosenParser =
                            chooseParserByName validated.parserName model.existingParsers
                    in
                    case maybeChosenParser of
                        Just chosenParser ->
                            ( CreateLogfileParser session
                                { requestState = model.requestState
                                , existingParsers = model.existingParsers
                                , chosenParsers = ( validated.columnName, chosenParser ) :: model.chosenParsers
                                , displayDropdown = model.displayDropdown
                                , parserName = model.parserName
                                , selectedParser = model.selectedParser
                                , nameForSelectedParser = ""
                                , stringToParse = model.stringToParse
                                , parsingResult = model.parsingResult
                                , problems = []
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            -- Should not be possible due to validation
                            ( CreateLogfileParser session model, Cmd.none )

                Err problems ->
                    ( CreateLogfileParser session { model | problems = problems }, Cmd.none )

        ChangeParsingContent newValue ->
            ( CreateLogfileParser session { model | stringToParse = newValue }, Cmd.none )

        ApplyParser target logfileParser ->
            case validateLogfileParser logfileParser of
                Ok validParser ->
                    ( CreateLogfileParser session { model | problems = [] }
                    , postApplyParser target validParser
                    )

                Err problems ->
                    ( CreateLogfileParser session { model | problems = problems }, Cmd.none )

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

        Submit logfileParser ->
            case validateLogfileParser logfileParser of
                Ok validParser ->
                    ( CreateLogfileParser session { model | problems = [] }
                    , postParser validParser
                    )

                Err problems ->
                    ( CreateLogfileParser session { model | problems = problems }, Cmd.none )


chooseParserByName : String -> List ElementaryParser.ElementaryParser -> Maybe ElementaryParser.ElementaryParser
chooseParserByName targetName parsers =
    let
        matchesName (ElementaryParser.ElementaryParser name _) =
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
                    , viewProblems model.problems
                    , div [ class "input-group", class "input-group--centered-content" ]
                        [ label [ for "parserNameInput" ] [ text "Name" ]
                        , input [ id "parserNameInput", placeholder "Logfile parser name", value model.parserName, onInput ChangeParserName ] [ text model.parserName ]
                        ]
                    , div [ class "input-group", class "input-group--centered-content" ]
                        [ label [ for "selectedParserName" ] [ text "ColumnName" ]
                        , input [ id "selectedParserName", placeholder "Name of the parsed value", value model.nameForSelectedParser, onInput ChangeNameForSelectedParser ]
                            [ text model.nameForSelectedParser ]
                        ]
                    , viewParserSelection model
                    , div [ class "button-group", class "button-group--centered-content" ]
                        [ button
                            [ onClick
                                (Submit
                                    { name = model.parserName
                                    , parsers = model.chosenParsers
                                    }
                                )
                            , class "standard-button"
                            ]
                            [ text "Submit" ]
                        ]
                    , ul [ class "parser-list" ] (List.map viewParser model.chosenParsers)
                    ]
                , viewParserApplication model
                ]


viewProblems : List ValidationProblem -> Html Msg
viewProblems problems =
    case problems of
        [] ->
            ul [] []

        _ ->
            ul [ class "problem-list" ]
                (List.map
                    (\(InvalidEntry _ problem) ->
                        li [ class "problem-list__element" ] [ text problem ]
                    )
                    problems
                )


viewParserSelection : CreateLogfileParserModel -> Html Msg
viewParserSelection model =
    div [ class "input-group", class "input-group--centered-content" ]
        [ label [] [ text "Parser" ]
        , select [ value model.selectedParser, onInput SelectParserToAdd ] (List.map (parserToOption model.selectedParser) model.existingParsers)
        , button
            [ onClick
                (AddSelectedParser
                    { columnName = model.nameForSelectedParser
                    , parserName = model.selectedParser
                    }
                )
            , class "standard-button"
            ]
            [ text "Apply" ]
        ]


viewParser : ( String, ElementaryParser.ElementaryParser ) -> Html Msg
viewParser ( name, ElementaryParser.ElementaryParser _ parser ) =
    case parser of
        ElementaryParser.OneOf xs ->
            li [] [ text (name ++ ": [ " ++ String.join ", " xs ++ " ]") ]

        ElementaryParser.Time pattern ->
            li [] [ text (name ++ ": " ++ pattern) ]

        ElementaryParser.Date pattern ->
            li [] [ text (name ++ ": " ++ pattern) ]

        ElementaryParser.Characters s ->
            li [] [ text (name ++ ": " ++ s) ]

        ElementaryParser.MatchUntilIncluded s ->
            li [] [ text (name ++ ": " ++ s) ]

        ElementaryParser.MatchUntilExcluded s ->
            li [] [ text (name ++ ": " ++ s) ]

        ElementaryParser.MatchFor number ->
            li [] [ text (name ++ ": " ++ String.fromInt number) ]

        ElementaryParser.MatchUntilEnd ->
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
            [ button
                [ onClick
                    (ApplyParser
                        model.stringToParse
                        { name = model.parserName, parsers = model.chosenParsers }
                    )
                , class "standard-button"
                , class "standard-button--long"
                ]
                [ text "Apply" ]
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
parserToOption selection (ElementaryParser.ElementaryParser name _) =
    option [ value name, selected (selection == name) ] [ text name ]



-- FORM


validateAddParserData : List ElementaryParser.ElementaryParser -> AddElementaryParserData -> Result (List ValidationProblem) (Validate.Valid AddElementaryParserData)
validateAddParserData existingParsers data =
    validate (addElementaryParserValidator existingParsers) data


validateLogfileParser : LogfileParser.LogfileParser -> Result (List ValidationProblem) (Validate.Valid LogfileParser.LogfileParser)
validateLogfileParser logfileParser =
    validate logfileParserValidator logfileParser


addElementaryParserValidator : List ElementaryParser.ElementaryParser -> Validator ValidationProblem AddElementaryParserData
addElementaryParserValidator existingParsers =
    let
        selectedParserExists =
            \model -> chooseParserByName (.parserName model) existingParsers /= Nothing
    in
    Validate.all
        [ ifBlank .columnName
            (InvalidEntry ColumnName "The column name for the parsing result musn't be empty.")
        , Validate.firstError
            [ ifBlank .parserName
                (InvalidEntry SelectedParserName "Please select an elementary parser.")
            , ifFalse selectedParserExists
                (InvalidEntry SelectedParserName "Please select a valid parser.")
            ]
        ]


logfileParserValidator : Validator ValidationProblem LogfileParser.LogfileParser
logfileParserValidator =
    Validate.firstError
        [ ifBlank .name
            (InvalidEntry LogfileParserName "Please enter a name for the logfile parser.")
        , ifEmptyList .parsers
            (InvalidEntry ChosenParsers "Please add at least one elementary parser to the logfile parser.")
        ]



-- HTTP


postParser : Validate.Valid LogfileParser.LogfileParser -> Cmd Msg
postParser validParser =
    let
        parser =
            Validate.fromValid validParser
    in
    Http.post
        { url = "http://localhost:8080/api/parsers/logfile"
        , body = Http.jsonBody (LogfileParser.logfileParserEncoder parser)
        , expect = Http.expectWhatever PostedLogfileParser
        }


postApplyParser : String -> Validate.Valid LogfileParser.LogfileParser -> Cmd Msg
postApplyParser target validParser =
    let
        parser =
            Validate.fromValid validParser

        data =
            { target = target, parser = parser }
    in
    Http.post
        { url = "http://localhost:8080/api/parsers/logfile/apply"
        , body = Http.jsonBody (ParserApplication.logfileParserApplicationEncoder data)
        , expect = Http.expectJson GotParserApplicationResult ParserApplication.logfileParserApplicationDecoder
        }
