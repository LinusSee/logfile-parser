module Page.ParserCreation exposing (..)

import DecEnc
import Html exposing (Html, a, article, button, div, h2, input, label, li, option, p, select, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Session exposing (Session)
import Validate exposing (Validator, fromErrors, ifBlank, ifFalse, ifTrue, validate)



-- MODEL


type Model
    = CreateParser Session CreateParserModel


type alias CreateParserModel =
    { requestState : HttpRequestState
    , createForm : DecEnc.ParserFormData
    , existingParsers : List DecEnc.ElementaryParser
    , parserToApply : String
    , stringToParse : String
    , parsingResult : String
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
            }
        , parserToApply = ""
        , stringToParse = ""
        , parsingResult = ""
        , existingParsers = []
        , problems = []
        }
    , Cmd.batch
        [ Http.get
            { url = "http://localhost:8080/api/parsers/building-blocks/complex"
            , expect = Http.expectJson GotElementaryParsers DecEnc.parsersDataDecoder
            }
        ]
    )



-- UPDATE


type Msg
    = GotElementaryParsers (Result Http.Error (List DecEnc.ElementaryParser))
    | PostedParser (Result Http.Error ())
    | ChangeForm FormChanged String
    | ChoseParserToApply String
    | ChangeParsingContent String
    | ApplyParser
    | GotParserApplicationResult (Result Http.Error String)
    | Reset
    | Submit DecEnc.ParserFormData


type FormChanged
    = ChangePatternType
    | ChangeMatching
    | ChangeName


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
                                    }
                                , parserToApply =
                                    case List.head data of
                                        Just (DecEnc.OneOf name _) ->
                                            name

                                        Just (DecEnc.Time name _) ->
                                            name

                                        Just (DecEnc.Date name _) ->
                                            name

                                        Just (DecEnc.Characters name _) ->
                                            name

                                        Nothing ->
                                            ""
                                , stringToParse = ""
                                , parsingResult = ""
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
                    Debug.log (Debug.toString error) ( CreateParser session { model | requestState = Failure }, Cmd.none )

        PostedParser result ->
            case result of
                Ok _ ->
                    ( CreateParser session model, Cmd.none )

                Err error ->
                    Debug.log (Debug.toString error) ( CreateParser session { model | requestState = Failure }, Cmd.none )

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

        ChoseParserToApply selection ->
            ( CreateParser session { model | parserToApply = selection }, Cmd.none )

        ChangeParsingContent value ->
            ( CreateParser session { model | stringToParse = value }, Cmd.none )

        ApplyParser ->
            ( CreateParser session model
            , postApplyParser model.stringToParse (chooseParserByName model.parserToApply model.existingParsers)
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
                    }
                , parserToApply = model.parserToApply
                , stringToParse = ""
                , parsingResult = ""
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
    in
    List.head (List.filter matchesName parsers)



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
                            ]
                        ]
                    , div [ class "input-group", class "input-group--centered-content" ]
                        [ label [ for "matchingInput" ] [ text "Matching" ]
                        , input [ id "matchingInput", placeholder "'a', 'b', 'c'", value formData.matching, onInput (ChangeForm ChangeMatching) ] []
                        ]
                    , div [ class "input-group", class "input-group--centered-content" ]
                        [ label [ for "parserNameInput" ] [ text "Name" ]
                        , input [ id "parserNameInput", placeholder "Loglevel oneof", value formData.name, onInput (ChangeForm ChangeName) ] []
                        ]
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
                , p [ class "text--centered" ] [ text model.parsingResult ]
                ]



-- viewParser : DecEnc.ElementaryParser -> Html Msg
-- viewParser parser =
--     case parser of
--         DecEnc.OneOf _ xs ->
--             li [] [ text ("[ " ++ String.join ", " xs ++ " ]") ]
--
--         DecEnc.Time _ pattern ->
--             li [] [ text pattern ]
--
--         DecEnc.Date _ pattern ->
--             li [] [ text pattern ]
--
--         DecEnc.Characters _ s ->
--             li [] [ text s ]


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


viewParserApplication : String -> List DecEnc.ElementaryParser -> String -> Html Msg
viewParserApplication selection parsers stringToParse =
    article [ class "article" ]
        [ h2 [ class "header2--centered" ] [ text "Test existing parsers" ]
        , div [ class "input-group", class "input-group--centered-content" ]
            [ label [] [ text "Parser" ]
            , select [ value selection, onInput ChoseParserToApply ] (List.map (parserToOption selection) parsers)
            ]
        , div [ class "input-group", class "input-group--centered-content" ]
            [ label [] [ text "Target" ]
            , input [ placeholder "String to parse", value stringToParse, onInput ChangeParsingContent ] []
            ]
        , div [ class "button-group", class "button-group--centered-content" ]
            [ button [ onClick ApplyParser, class "standard-button", class "standard-button--long" ] [ text "Apply" ] ]
        ]


parserToOption : String -> DecEnc.ElementaryParser -> Html Msg
parserToOption selection parser =
    case parser of
        DecEnc.OneOf name elements ->
            option [ value name, selected (selection == name) ] [ text name ]

        DecEnc.Time name pattern ->
            option [ value name, selected (selection == name) ] [ text name ]

        DecEnc.Date name pattern ->
            option [ value name, selected (selection == name) ] [ text name ]

        DecEnc.Characters name chars ->
            option [ value name, selected (selection == name) ] [ text name ]



-- FORM


type alias ValidatedModel =
    { matching : String, name : String }


validateForm : DecEnc.ParserFormData -> Result (List ValidationProblem) (Validate.Valid ValidatedModel)
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

        _ ->
            Debug.todo "Should never happen"


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



-- oneOfMatchingToProblems : String -> List ValidationProblem
-- oneOfMatchingToProblems input =
--     let
--         inputList =
--             String.split "," input
--
--         validElement element =
--             let
--                 trimmedElement =
--                     String.trim element
--             in
--             String.startsWith "\"" trimmedElement && String.endsWith "\"" trimmedElement
--     in
--     if List.member True (List.map validElement inputList) then
--         [ InvalidEntry Matching "Each element must be a string enclosed by \"" ]
--
--     else
--         []
-- HTTP


postParser : DecEnc.ParserFormData -> Cmd Msg
postParser formData =
    Http.post
        { url = "http://localhost:8080/api/parsers/building-blocks/complex"
        , body = Http.jsonBody (DecEnc.parserEncoder formData)
        , expect = Http.expectWhatever PostedParser
        }


postApplyParser : String -> Maybe DecEnc.ElementaryParser -> Cmd Msg
postApplyParser target maybeParser =
    case maybeParser of
        Just parser ->
            let
                data =
                    { target = target, parser = parser }
            in
            Http.post
                { url = "http://localhost:8080/api/parsers/building-blocks/complex/apply"
                , body = Http.jsonBody (DecEnc.parserApplicationEncoder data)
                , expect = Http.expectJson GotParserApplicationResult DecEnc.parserApplicationDecoder
                }

        Nothing ->
            Cmd.none
