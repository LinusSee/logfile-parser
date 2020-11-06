module Page.ParserCreation exposing (..)

import DecEnc
import Html exposing (Html, a, button, div, h2, input, label, li, option, select, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Session exposing (Session)
import Validate exposing (Validator, ifBlank, validate)



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
                    , div []
                        [ case validate modelValidator { matching = formData.matching, name = formData.name } of
                            Ok _ ->
                                text "Successful validaton"

                            Err errs ->
                                ul [] (List.map (\(InvalidEntry _ err) -> li [] [ text err ]) errs)
                        ]
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



-- FORM
-- type FormField
--     = Matching
--     | Name
--
--
-- type alias Error =
--     ( FormField, String )
-- validate : CreateParserModel -> List Error
-- validate =
--     Validate.all
--         [ .matching >> Validate.ifBlank ( Matching, "Matching pattern musn't be empty." )
--         , .name >> Validate.ifBlank ( Name, "Name musn't be empty." )
--         ]


type alias ValidatedModel =
    { matching : String, name : String }


validateForm : DecEnc.ParserFormData -> Result (List ValidationProblem) (Validate.Valid ValidatedModel)
validateForm formData =
    validate modelValidator { matching = formData.matching, name = formData.name }


modelValidator : Validator ValidationProblem ValidatedModel
modelValidator =
    Validate.all
        [ ifBlank .matching (InvalidEntry Matching "Matching pattern musn't be empty.")
        , ifBlank .name (InvalidEntry Name "Name musn't be empty.")
        ]



-- HTTP


postParser : DecEnc.ParserFormData -> Cmd Msg
postParser formData =
    Http.post
        { url = "http://localhost:8080/api/parsers/building-blocks/complex"
        , body = Http.jsonBody (DecEnc.parserEncoder formData)
        , expect = Http.expectWhatever PostedParser
        }
