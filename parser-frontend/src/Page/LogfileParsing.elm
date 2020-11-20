module Page.LogfileParsing exposing (..)

import DecEnc
import Html exposing (Html, button, div, h2, input, label, li, option, select, text, ul)
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
    , chosenParsers : List DecEnc.ElementaryParser
    , displayDropdown : Bool
    , parserName : String
    , selectedParser : String
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
    | AddSelectedParser
    | PostedLogfileParser (Result Http.Error ())
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

                                        Nothing ->
                                            ""
                                }
                            , Cmd.none
                            )

                        Success ->
                            ( CreateLogfileParser session { model | existingParsers = data }, Cmd.none )

                Err error ->
                    Debug.log (Debug.toString error) ( CreateLogfileParser session { model | requestState = Failure }, Cmd.none )

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

        AddSelectedParser ->
            let
                maybeChosenParser =
                    chooseParserByName model.selectedParser model.existingParsers
            in
            case maybeChosenParser of
                Just chosenParser ->
                    ( CreateLogfileParser
                        session
                        { model | chosenParsers = chosenParser :: model.chosenParsers }
                    , Cmd.none
                    )

                Nothing ->
                    ( CreateLogfileParser session model, Cmd.none )

        PostedLogfileParser response ->
            case response of
                Ok _ ->
                    ( CreateLogfileParser session model, Cmd.none )

                Err error ->
                    Debug.log (Debug.toString error) ( CreateLogfileParser session { model | requestState = Failure }, Cmd.none )

        Submit ->
            ( CreateLogfileParser session model, postParser model.parserName model.chosenParsers )


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


view : CreateLogfileParserModel -> Html Msg
view model =
    case model.requestState of
        Failure ->
            div [] [ text "Failed to load data" ]

        Loading ->
            div [] [ text "Loading..." ]

        Success ->
            div []
                [ h2 [] [ text "Combine parsers to create a logfile parser" ]
                , div []
                    [ label []
                        [ text "Choose your parser name"
                        , input [ placeholder "Parser name", value model.parserName, onInput ChangeParserName ] [ text model.parserName ]
                        ]
                    , ul [] (List.map viewParser model.chosenParsers)
                    , viewParserSelection model.selectedParser model.existingParsers
                    ]
                , button [ onClick Submit ] [ text "Submit" ]
                ]


viewParserSelection : String -> List DecEnc.ElementaryParser -> Html Msg
viewParserSelection selection parsers =
    div []
        [ label []
            [ text "Parser"
            , select [ value selection, onInput SelectParserToAdd ] (List.map (parserToOption selection) parsers)
            ]
        , button [ onClick AddSelectedParser ] [ text "Apply" ]
        ]


viewParser : DecEnc.ElementaryParser -> Html Msg
viewParser parser =
    case parser of
        DecEnc.OneOf _ xs ->
            li [] [ text ("[ " ++ String.join ", " xs ++ " ]") ]

        DecEnc.Time _ pattern ->
            li [] [ text pattern ]

        DecEnc.Date _ pattern ->
            li [] [ text pattern ]

        DecEnc.Characters _ s ->
            li [] [ text s ]


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



-- HTTP


postParser : String -> List DecEnc.ElementaryParser -> Cmd Msg
postParser name logfileParser =
    Http.post
        { url = "http://localhost:8080/api/parsers/logfile"
        , body = Http.jsonBody (DecEnc.logfileParserEncoder name logfileParser)
        , expect = Http.expectWhatever PostedLogfileParser
        }
