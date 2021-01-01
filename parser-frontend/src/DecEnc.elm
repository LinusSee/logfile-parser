module DecEnc exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, field, int, map3, string)
import Json.Encode as Encode
import Models.Shared.ElementaryParser as ElementaryParser



-- DATA


type alias ParserFormData =
    { patternType : String
    , matching : String
    , name : String
    }


type alias ParserApplicationData =
    { target : String
    , parser : ElementaryParser.ElementaryParser
    }


type alias LogfileParser =
    { name : String
    , parsers : List ( String, ElementaryParser.ElementaryParser )
    }


type alias LogfileParserApplicationData =
    { target : String
    , parser : LogfileParser
    }



-- HTTP
-- Maybe TEMP


toMatchingList : String -> List String
toMatchingList matching =
    let
        inputList =
            String.split "," matching

        cleanupElement element =
            String.trim element |> String.slice 1 -1
    in
    List.map cleanupElement inputList


parsersDataDecoder : Decoder (List ElementaryParser.ElementaryParser)
parsersDataDecoder =
    Decode.list ElementaryParser.parserDataDecoder


parserEncoder : ParserFormData -> Encode.Value
parserEncoder formData =
    case formData.patternType of
        "oneOf" ->
            ElementaryParser.oneOfEncoder formData.name (toMatchingList formData.matching)

        "time" ->
            ElementaryParser.timeEncoder formData.name formData.matching

        "date" ->
            ElementaryParser.dateEncoder formData.name formData.matching

        "characters" ->
            ElementaryParser.charactersEncoder formData.name formData.matching

        "matchUntilIncluded" ->
            ElementaryParser.matchUntilIncludedEncoder formData.name formData.matching

        "matchUntilExcluded" ->
            ElementaryParser.matchUntilExcludedEncoder formData.name formData.matching

        "matchFor" ->
            case String.toInt formData.matching of
                Just number ->
                    ElementaryParser.matchForEncoder formData.name number

                Nothing ->
                    Encode.object
                        [ ( "type", Encode.string "invalidType" )
                        , ( "name", Encode.string "invalidName" )
                        , ( "value", Encode.string "invalidValue" )
                        ]

        -- TEMP: Need to find out how to solve this
        _ ->
            Encode.object
                [ ( "type", Encode.string "invalidType" )
                , ( "name", Encode.string "invalidName" )
                , ( "value", Encode.string "invalidValue" )
                ]


parserApplicationEncoder : ParserApplicationData -> Encode.Value
parserApplicationEncoder data =
    Encode.object
        [ ( "target", Encode.string data.target )
        , ( "parser", ElementaryParser.elementaryParserEncoder data.parser )
        ]


logfileParserEncoder : LogfileParser -> Encode.Value
logfileParserEncoder parser =
    Encode.object
        [ ( "name", Encode.string parser.name )
        , ( "parsers", Encode.list namedParserEncoder parser.parsers )
        ]


namedParserEncoder : ( String, ElementaryParser.ElementaryParser ) -> Encode.Value
namedParserEncoder ( name, parser ) =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "parser", ElementaryParser.elementaryParserEncoder parser )
        ]


logfileParserApplicationEncoder : LogfileParserApplicationData -> Encode.Value
logfileParserApplicationEncoder data =
    Encode.object
        [ ( "target", Encode.string data.target )
        , ( "parser", logfileParserEncoder data.parser )
        ]


parserApplicationDecoder : Decoder ( String, String )
parserApplicationDecoder =
    Decode.map2 Tuple.pair
        (field "name" string)
        (field "result" string)


logfileParserApplicationDecoder : Decoder (List (List ( String, String )))
logfileParserApplicationDecoder =
    field "result" (Decode.list (Decode.list parserApplicationDecoder))


logfileParserNames : Decoder (List String)
logfileParserNames =
    Decode.list string
