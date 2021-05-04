module Page.ParserCreation exposing (..)

import Html exposing (Html, a, article, button, div, h2, input, label, li, option, p, select, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Models.Shared.ElementaryParser as ElementaryParser
import Models.Shared.ParserApplication as ParserApplication
import Models.Specific.ParserCreation as PageModels
import Session exposing (Session)
import Url.Builder as UrlBuilder
import Validate exposing (Validator, fromErrors, ifBlank, ifFalse, ifTrue, validate)



-- MODEL


type Model
    = CreateParser Session CreateParserModel


type alias CreateParserModel =
    { requestState : HttpRequestState
    , createForm : PageModels.ParserFormData
    , existingParsers : List ElementaryParser.ElementaryParserId
    , parserToApply : Maybe ElementaryParser.ElementaryParserId
    , stringToParse : String
    , parsingResult : ( String, String )
    , problems : List ValidationProblem
    }


type HttpRequestState
    = Failure
    | Loading
    | Success


type ValidationProblem
    = InvalidEntry ValidatedField String


type ValidatedField
    = Matching
    | PatternType
    | Name


init : Session -> ( Model, Cmd Msg )
init session =
    ( CreateParser
        session
        { requestState = Loading
        , createForm =
            { patternType = "oneOf"
            , matching = ""
            , name = ""
            , parsingOptions = defaultParsingOptions
            }
        , parserToApply = Nothing
        , stringToParse = ""
        , parsingResult = ( "", "" )
        , existingParsers = []
        , problems = []
        }
    , Cmd.batch
        [ Http.get
            { url = "http://localhost:8080/api/parsers/building-blocks/complex/ids"
            , expect = Http.expectJson GotElementaryParsers ElementaryParser.parserIdsDecoder
            }
        ]
    )



-- UPDATE


type Msg
    = GotElementaryParsers (Result Http.Error (List ElementaryParser.ElementaryParserId))
    | PostedParser (Result Http.Error ())
    | ChangeForm FormChanged String
    | ChoseParserToApply String
    | ChangeParsingContent String
    | ApplyParser
    | GotParserApplicationResult (Result Http.Error ( String, String ))
    | Reset
    | Submit PageModels.ParserFormData


type FormChanged
    = ChangePatternType
    | ChangeMatching
    | ChangeName
    | ChangeKeepResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (CreateParser session model) =
    case msg of
        GotElementaryParsers result ->
            case result of
                Ok data ->
                    case model.requestState of
                        Failure ->
                            ( CreateParser session { model | requestState = Failure }, Cmd.none )

                        Loading ->
                            ( CreateParser session
                                { requestState = Success
                                , createForm =
                                    { patternType = "oneOf"
                                    , matching = ""
                                    , name = ""
                                    , parsingOptions = defaultParsingOptions
                                    }
                                , parserToApply = List.head data
                                , stringToParse = ""
                                , parsingResult = ( "", "" )
                                , existingParsers = data
                                , problems = []
                                }
                            , Cmd.none
                            )

                        Success ->
                            ( CreateParser session
                                { model | existingParsers = data }
                            , Cmd.none
                            )

                Err error ->
                    ( CreateParser session { model | requestState = Failure }, Cmd.none )

        PostedParser result ->
            case result of
                Ok _ ->
                    ( CreateParser session model, Cmd.none )

                Err error ->
                    ( CreateParser session { model | requestState = Failure }, Cmd.none )

        ChangeForm field newContent ->
            case model.requestState of
                Failure ->
                    ( CreateParser session model, Cmd.none )

                Loading ->
                    ( CreateParser session { model | requestState = Failure }, Cmd.none )

                Success ->
                    case field of
                        ChangePatternType ->
                            let
                                formData =
                                    model.createForm
                            in
                            ( CreateParser session
                                { model | createForm = { formData | patternType = newContent } }
                            , Cmd.none
                            )

                        ChangeMatching ->
                            let
                                formData =
                                    model.createForm
                            in
                            ( CreateParser session
                                { model | createForm = { formData | matching = newContent } }
                            , Cmd.none
                            )

                        ChangeName ->
                            let
                                formData =
                                    model.createForm
                            in
                            ( CreateParser session
                                { model | createForm = { formData | name = newContent } }
                            , Cmd.none
                            )

                        ChangeKeepResult ->
                            let
                                formData =
                                    model.createForm

                                parsingOptions =
                                    formData.parsingOptions
                            in
                            ( CreateParser session
                                { model | createForm = { formData | parsingOptions = { parsingOptions | keepResult = not formData.parsingOptions.keepResult } } }
                              --{ model | createForm = { formData | parsingOptions = toggleParsingOption (ElementaryParser.KeepResult True) formData.parsingOptions } }
                            , Cmd.none
                            )

        ChoseParserToApply selectedId ->
            ( CreateParser session { model | parserToApply = List.head (List.filter (\parserId -> parserId.parserId == selectedId) model.existingParsers) }, Cmd.none )

        ChangeParsingContent value ->
            ( CreateParser session { model | stringToParse = value }, Cmd.none )

        ApplyParser ->
            ( CreateParser session model
            , getApplyParser model
            )

        GotParserApplicationResult response ->
            case response of
                Ok result ->
                    ( CreateParser session { model | parsingResult = result }, Cmd.none )

                Err _ ->
                    ( CreateParser session model, Cmd.none )

        Reset ->
            ( CreateParser session
                { requestState = model.requestState
                , createForm =
                    { patternType = "oneOf"
                    , matching = ""
                    , name = ""
                    , parsingOptions = defaultParsingOptions
                    }
                , parserToApply = model.parserToApply
                , stringToParse = ""
                , parsingResult = ( "", "" )
                , existingParsers = model.existingParsers
                , problems = []
                }
            , Cmd.none
            )

        Submit formData ->
            case validateForm formData of
                Ok _ ->
                    ( CreateParser session model
                    , postParser formData
                    )

                Err problems ->
                    ( CreateParser session { model | problems = problems }
                    , Cmd.none
                    )


chooseParserByName : String -> List ElementaryParser.ElementaryParser -> Maybe ElementaryParser.ElementaryParser
chooseParserByName targetName parsers =
    let
        matchesName (ElementaryParser.ElementaryParser name _ _) =
            targetName == name
    in
    List.head (List.filter matchesName parsers)


defaultParsingOptions : ElementaryParser.ParsingOptions
defaultParsingOptions =
    { keepResult = True }



-- test : ElementaryParser.ParsingOption -> ElementaryParser.ParsingOption -> Bool
-- test newOption oldOption =
--     case ( newOption, oldOption ) of
--         ( ElementaryParser.KeepResult _, ElementaryParser.KeepResult _ ) ->
--             True
--
--         _ ->
--             False
-- toggleParsingOption : ElementaryParser.ParsingOption -> ElementaryParser.ParsingOptions -> ElementaryParser.ParsingOptions
-- toggleParsingOption option (ElementaryParser.ParsingOptions options) =
--     let
--         mapOptions newOption oldOption =
--             case ( newOption, oldOption ) of
--                 ( ElementaryParser.KeepResult _, ElementaryParser.KeepResult oldResult ) ->
--                     ElementaryParser.KeepResult (not oldResult)
--     in
--     ElementaryParser.ParsingOptions (List.map (mapOptions option) options)
-- VIEW


view : CreateParserModel -> Html Msg
view model =
    case model.requestState of
        Failure ->
            div [] [ text "Failed to load data" ]

        Loading ->
            div [] [ text "Loading..." ]

        Success ->
            let
                formData =
                    model.createForm

                existingParsers =
                    model.existingParsers
            in
            div []
                [ article [ class "article" ]
                    [ h2 [ class "header2--centered" ] [ text "Create specialized parsers" ]
                    , viewProblems model.problems
                    , div [ class "input-group", class "input-group--centered-content" ]
                        [ label [ for "typeSelect" ] [ text "Type" ]
                        , select [ id "typeSelect", value formData.patternType, onInput (ChangeForm ChangePatternType) ]
                            [ option [ value "oneOf", selected (formData.patternType == "oneOf") ] [ text "One Of" ]
                            , option [ value "date", selected (formData.patternType == "date") ] [ text "Date" ]
                            , option [ value "time", selected (formData.patternType == "time") ] [ text "Time" ]
                            , option [ value "characters", selected (formData.patternType == "characters") ] [ text "String" ]
                            , option [ value "matchUntilIncluded", selected (formData.patternType == "matchUntilIncluded") ] [ text "Match Until Included" ]
                            , option [ value "matchUntilExcluded", selected (formData.patternType == "matchUntilExcluded") ] [ text "Match Until Excluded" ]
                            , option [ value "matchFor", selected (formData.patternType == "matchFor") ] [ text "Match For" ]
                            ]
                        ]
                    , div [ class "input-group", class "input-group--centered-content" ]
                        [ label [ for "matchingInput" ] [ text "Matching" ]
                        , input [ id "matchingInput", placeholder (matchingPlaceholder formData.patternType), value formData.matching, onInput (ChangeForm ChangeMatching) ] []
                        ]
                    , div [ class "input-group", class "input-group--centered-content" ]
                        [ label [ for "parserNameInput" ] [ text "Name" ]
                        , input [ id "parserNameInput", placeholder (namePlaceholder formData.patternType), value formData.name, onInput (ChangeForm ChangeName) ] []
                        ]
                    , viewParsingOptions formData.parsingOptions
                    , div [ class "button-group", class "button-group--centered-content" ]
                        [ button
                            [ onClick Reset
                            , class "standard-button"
                            , class "standard-button--spacing"
                            , class "standard-button--long"
                            ]
                            [ text "Reset" ]
                        , button
                            [ onClick (Submit formData)
                            , class "standard-button"
                            , class "standard-button--spacing"
                            , class "standard-button--long"
                            ]
                            [ text "Submit" ]
                        ]

                    -- , ul [] (List.map viewParser existingParsers)
                    ]
                , viewParserApplication model.parserToApply existingParsers model.stringToParse
                , p [ class "text--centered" ] [ text (Tuple.second model.parsingResult) ]
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


viewParserApplication : Maybe ElementaryParser.ElementaryParserId -> List ElementaryParser.ElementaryParserId -> String -> Html Msg
viewParserApplication maybeSelection parserIds stringToParse =
    article [ class "article" ]
        [ h2 [ class "header2--centered" ] [ text "Test existing parsers" ]
        , div [ class "input-group", class "input-group--centered-content" ]
            [ label [] [ text "Parser" ]
            , case maybeSelection of
                Just selection ->
                    select [ value selection.parserId, onInput ChoseParserToApply ] (List.map (parserToOption selection.parserId) parserIds)

                Nothing ->
                    select [ value "", onInput ChoseParserToApply ] (List.map (parserToOption "") parserIds)
            ]
        , div [ class "input-group", class "input-group--centered-content" ]
            [ label [] [ text "Target" ]
            , input [ placeholder "String to parse", value stringToParse, onInput ChangeParsingContent ] []
            ]
        , div [ class "button-group", class "button-group--centered-content" ]
            [ button [ onClick ApplyParser, class "standard-button", class "standard-button--long" ] [ text "Apply" ] ]
        ]


parserToOption : String -> ElementaryParser.ElementaryParserId -> Html Msg
parserToOption selection parserId =
    option [ value parserId.parserId, selected (selection == parserId.parserId) ] [ text parserId.name ]


viewParsingOptions : ElementaryParser.ParsingOptions -> Html Msg
viewParsingOptions options =
    div []
        [ div [ class "input-group", class "input-group--centered-content" ]
            [ label [ for "keepResult" ] [ text "Keep result" ]
            , input [ id "keepResult", type_ "checkbox", checked options.keepResult, onInput (ChangeForm ChangeKeepResult) ] []
            ]
        ]


matchingPlaceholder : String -> String
matchingPlaceholder selection =
    case selection of
        "oneOf" ->
            "a list e.g. \"a\", \"asdf\", \"foo\""

        "time" ->
            "e.g. HH:MM"

        "date" ->
            "e.g. YYYY-MM-DD"

        "characters" ->
            "an arbitrary string"

        "matchUntilIncluded" ->
            "e.g. <StartCorrelationId>"

        "matchUntilExcluded" ->
            "e.g. <EndCorrelationId>"

        _ ->
            "The number of chars e.g. 5"


namePlaceholder : String -> String
namePlaceholder selection =
    case selection of
        "oneOf" ->
            "oneOf Loglevel"

        "time" ->
            "Standard Time"

        "date" ->
            "Easy sort date"

        "characters" ->
            "<EndCorrelationId>"

        "matchUntilIncluded" ->
            "UntilCorrelationId"

        "matchUntilExcluded" ->
            "ExceptEndCorrelationId"

        _ ->
            "Next 5 chars"



-- FORM


type alias ValidatedModel =
    { matching : String, name : String }


validateForm : PageModels.ParserFormData -> Result (List ValidationProblem) (Validate.Valid ValidatedModel)
validateForm formData =
    validate (modelValidator formData.patternType) { matching = formData.matching, name = formData.name }


modelValidator : String -> Validator ValidationProblem ValidatedModel
modelValidator patternType =
    Validate.all
        [ matchingValidator patternType
        , ifBlank .name (InvalidEntry Name "Name musn't be empty.")
        ]


matchingValidator : String -> Validator ValidationProblem ValidatedModel
matchingValidator patternType =
    -- TODO: Maybe change this to a type? Would make handling easier
    case patternType of
        "oneOf" ->
            Validate.firstError
                [ ifBlank .matching (InvalidEntry Matching "OneOf matching pattern must have at least one element.")
                , ifTrue isInvalidList (InvalidEntry Matching "Each element must be a string enclosed by \"")
                ]

        "date" ->
            Validate.firstError
                [ ifBlank .matching (InvalidEntry Matching "Date matching pattern musn't be empty.")
                , ifFalse isValidDate (InvalidEntry Matching "Incorrect pattern for type 'date'.")
                ]

        "time" ->
            Validate.firstError
                [ ifBlank .matching (InvalidEntry Matching "Time matching pattern musn't be empty.")
                , ifFalse isValidTime (InvalidEntry Matching "Incorrect pattern for type 'time'.")
                ]

        "characters" ->
            ifBlank .matching (InvalidEntry Matching "Characters matching pattern musn't be empty.")

        "matchUntilIncluded" ->
            ifBlank .matching (InvalidEntry Matching "MatchUntilIncluded matching pattern musn't be empty.")

        "matchUntilExcluded" ->
            ifBlank .matching (InvalidEntry Matching "MatchUntilExcluded matching pattern musn't be empty.")

        "matchFor" ->
            ifBlank .matching (InvalidEntry Matching "MatchFor matching pattern musn't be empty.")

        _ ->
            ifTrue (\model -> True) (InvalidEntry PatternType "This pattern type does not exist.")


isInvalidList : ValidatedModel -> Bool
isInvalidList input =
    let
        inputList =
            String.split "," input.matching

        validElement element =
            let
                trimmedElement =
                    String.trim element
            in
            not
                (String.startsWith "\"" trimmedElement
                    && String.endsWith "\"" trimmedElement
                )
                || (String.length trimmedElement < 3)
    in
    List.member True (List.map validElement inputList)


isValidDate : ValidatedModel -> Bool
isValidDate model =
    let
        matching =
            model.matching
    in
    String.contains "yyyy" matching
        && String.contains "mm" matching
        && String.contains "dd" matching
        && (String.length matching == 10)


isValidTime : ValidatedModel -> Bool
isValidTime model =
    let
        matching =
            model.matching
    in
    String.contains "hh" matching
        && String.contains "mm" matching
        && (String.length matching == 5)



-- HTTP


postParser : PageModels.ParserFormData -> Cmd Msg
postParser formData =
    Http.post
        { url = "http://localhost:8080/api/parsers/building-blocks/complex"
        , body = Http.jsonBody (PageModels.parserEncoder formData)
        , expect = Http.expectWhatever PostedParser
        }


getApplyParser : CreateParserModel -> Cmd Msg
getApplyParser model =
    case model.parserToApply of
        Just parserId ->
            Http.get
                { url =
                    UrlBuilder.crossOrigin
                        "http://localhost:8080/api/parsers/building-blocks/complex/apply"
                        [ parserId.parserId ]
                        [ UrlBuilder.string "target" model.stringToParse ]
                , expect = Http.expectJson GotParserApplicationResult ParserApplication.parserApplicationDecoder
                }

        Nothing ->
            Debug.todo "No parser selected: Should not be possible and the user should be given a message"
