module Page.ParserCreation exposing (..)

import DecEnc
import Html exposing (Html, a, button, div, h2, input, label, li, option, select, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Session exposing (Session)



-- MODEL


type Model
    = CreateParser Session CreateParserModel


type alias CreateParserModel =
    { requestState : HttpRequestState
    }


type HttpRequestState
    = Failure
    | Loading
    | Success DecEnc.ParserFormData DecEnc.SampleData (List DecEnc.ElementaryParser)



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
