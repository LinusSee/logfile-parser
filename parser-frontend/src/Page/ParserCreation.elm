module Page.ParserCreation exposing (..)

import DecEnc
import Html exposing (Html, a, button, div, h2, input, label, li, option, select, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Session exposing (Session)
import Validate exposing (Validator, fromErrors, ifBlank, ifTrue, validate)



-- MODEL


type Model
    = CreateParser Session CreateParserModel


type alias CreateParserModel =
    { requestState : HttpRequestState
    , problems : List ValidationProblem
    }


type HttpRequestState
    = Failure
    | Loading
    | Success DecEnc.ParserFormData DecEnc.SampleData (List DecEnc.ElementaryParser)


type ValidationProblem
    = InvalidEntry ValidatedField String


type ValidatedField
    = Matching
    | Name


init : Session -> ( Model, Cmd Msg )
init session =
    ( CreateParser session { requestState = Loading, problems = [] }
    , Cmd.batch
        [ Http.get
            { url = "http://localhost:8080/api/sample"
            , expect = Http.expectJson GotDummyData DecEnc.sampleDataDecoder
            }
        , Http.get
            { url = "http://localhost:8080/api/parsers/building-blocks/complex"
            , expect = Http.expectJson GotElementaryParsers DecEnc.parsersDataDecoder
            }
        ]
    )



-- UPDATE


type Msg
    = GotDummyData (Result Http.Error DecEnc.SampleData)
    | GotElementaryParsers (Result Http.Error (List DecEnc.ElementaryParser))
    | PostedParser (Result Http.Error ())
    | ChangeForm FormChanged String
    | Reset
    | Submit DecEnc.ParserFormData


type FormChanged
    = ChangePatternType
    | ChangeMatching
    | ChangeName


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (CreateParser session model) =
    case msg of
        GotDummyData result ->
            case result of
                Ok data ->
                    case model.requestState of
                        Failure ->
                            ( CreateParser session model, Cmd.none )

                        Loading ->
                            ( CreateParser session
                                { model
                                    | requestState =
                                        Success
                                            { patternType = "oneOf"
                                            , matching = ""
                                            , name = ""
                                            }
                                            data
                                            []
                                }
                            , Cmd.none
                            )

                        Success formData _ existingParsers ->
                            ( CreateParser
                                session
                                { model
                                    | requestState =
                                        Success
                                            formData
                                            data
                                            existingParsers
                                }
                            , Cmd.none
                            )

                Err error ->
                    Debug.log (Debug.toString error) ( CreateParser session { model | requestState = Failure }, Cmd.none )

        GotElementaryParsers result ->
            case result of
                Ok data ->
                    case model.requestState of
                        Failure ->
                            ( CreateParser session { model | requestState = Failure }, Cmd.none )

                        Loading ->
                            ( CreateParser session
                                { model
                                    | requestState =
                                        Success
                                            { patternType = "oneOf"
                                            , matching = ""
                                            , name = ""
                                            }
                                            { val1 = 0
                                            , val2 = ""
                                            , val3 = ""
                                            }
                                            data
                                }
                            , Cmd.none
                            )

                        Success formData loadedData _ ->
                            ( CreateParser session
                                { model
                                    | requestState =
                                        Success
                                            formData
                                            loadedData
                                            data
                                }
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

                Success formData loadedData existingParsers ->
                    case field of
                        ChangePatternType ->
                            ( CreateParser session
                                { model
                                    | requestState =
                                        Success { formData | patternType = newContent }
                                            loadedData
                                            existingParsers
                                }
                            , Cmd.none
                            )

                        ChangeMatching ->
                            ( CreateParser session
                                { model
                                    | requestState =
                                        Success { formData | matching = newContent }
                                            loadedData
                                            existingParsers
                                }
                            , Cmd.none
                            )

                        ChangeName ->
                            ( CreateParser session
                                { model
                                    | requestState =
                                        Success { formData | name = newContent }
                                            loadedData
                                            existingParsers
                                }
                            , Cmd.none
                            )

        Reset ->
            ( CreateParser session
                { model
                    | requestState =
                        Success
                            { patternType = "oneOf"
                            , matching = ""
                            , name = ""
                            }
                            { val1 = 1
                            , val2 = ""
                            , val3 = ""
                            }
                            []
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



-- VIEW


view : CreateParserModel -> Html Msg
view model =
    case model.requestState of
        Failure ->
            div [] [ text "Failed to load data" ]

        Loading ->
            div [] [ text "Loading..." ]

        Success formData loadedData existingParsers ->
            div []
                [ div []
                    [ h2 [] [ text "Create specialized parsers" ]
                    , viewProblems model.problems
                    , div []
                        [ label []
                            [ text "Type"
                            , select [ value formData.patternType, onInput (ChangeForm ChangePatternType) ]
                                [ option [ value "oneOf", selected (formData.patternType == "oneOf") ] [ text "One Of" ]
                                , option [ value "date", selected (formData.patternType == "date") ] [ text "Date" ]
                                , option [ value "time", selected (formData.patternType == "time") ] [ text "Time" ]
                                , option [ value "characters", selected (formData.patternType == "characters") ] [ text "String" ]
                                ]
                            ]
                        , label []
                            [ text "Matching"
                            , input [ placeholder "'a', 'b', 'c'", value formData.matching, onInput (ChangeForm ChangeMatching) ] []
                            ]
                        ]
                    , div []
                        [ label []
                            [ text "Name"
                            , input [ placeholder "Loglevel oneof", value formData.name, onInput (ChangeForm ChangeName) ] []
                            ]
                        ]
                    , div []
                        [ button [ onClick Reset ] [ text "Reset" ]
                        , button [ onClick (Submit formData) ] [ text "Submit" ]
                        ]
                    , text ("Loaded this string: " ++ loadedData.val2)
                    , ul [] (List.map viewParser existingParsers)
                    ]
                , a [ href "https://wikipedia.org" ] [ text "External link" ]
                , a [ href "http://localhost:8081/parse-logfile" ] [ text "Internal link" ]
                ]


viewParser : DecEnc.ElementaryParser -> Html Msg
viewParser parser =
    case parser of
        DecEnc.OneOf xs ->
            li [] [ text ("[ " ++ String.join ", " xs ++ " ]") ]

        DecEnc.Time pattern ->
            li [] [ text pattern ]

        DecEnc.Date pattern ->
            li [] [ text pattern ]

        DecEnc.Characters s ->
            li [] [ text s ]


viewProblems : List ValidationProblem -> Html Msg
viewProblems problems =
    div []
        [ case problems of
            [] ->
                text "Successful validaton"

            _ ->
                ul [] (List.map (\(InvalidEntry _ problem) -> li [] [ text problem ]) problems)
        ]



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
            ifBlank .matching (InvalidEntry Matching "Date matching pattern musn't be empty.")

        "time" ->
            ifBlank .matching (InvalidEntry Matching "Time matching pattern musn't be empty.")

        "characters" ->
            ifBlank .matching (InvalidEntry Matching "Characters matching pattern musn't be empty.")

        _ ->
            Debug.todo "Should never happen"



-- ifTrue True (InvalidEntry Matching "Invalid selection. Please select a valid type.")


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
