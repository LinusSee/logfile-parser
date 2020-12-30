module DecEnc exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, field, int, map3, string)
import Json.Encode as Encode



-- DATA


type alias TimePattern =
    String


type alias DatePattern =
    String


type ElementaryParser
    = OneOf String (List String)
    | Time String TimePattern
    | Date String DatePattern
    | Characters String String
    | MatchUntilIncluded String String
    | MatchUntilExcluded String String
    | MatchFor String Int
    | MatchUntilEnd String


type alias ParserFormData =
    { patternType : String
    , matching : String
    , name : String
    }


type alias ParserApplicationData =
    { target : String
    , parser : ElementaryParser
    }


type alias LogfileParser =
    { name : String
    , parsers : List ( String, ElementaryParser )
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


parserEncoder : ParserFormData -> Encode.Value
parserEncoder formData =
    case formData.patternType of
        "oneOf" ->
            oneOfEncoder formData.name (toMatchingList formData.matching)

        "time" ->
            timeEncoder formData.name formData.matching

        "date" ->
            dateEncoder formData.name formData.matching

        "characters" ->
            charactersEncoder formData.name formData.matching

        "matchUntilIncluded" ->
            matchUntilIncludedEncoder formData.name formData.matching

        "matchUntilExcluded" ->
            matchUntilExcludedEncoder formData.name formData.matching

        "matchFor" ->
            case String.toInt formData.matching of
                Just number ->
                    matchForEncoder formData.name number

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
        , ( "parser", elementaryParserEncoder data.parser )
        ]


elementaryParserEncoder : ElementaryParser -> Encode.Value
elementaryParserEncoder parser =
    case parser of
        OneOf name values ->
            oneOfEncoder name values

        Time name pattern ->
            timeEncoder name pattern

        Date name pattern ->
            dateEncoder name pattern

        Characters name value ->
            charactersEncoder name value

        MatchUntilIncluded name value ->
            matchUntilIncludedEncoder name value

        MatchUntilExcluded name value ->
            matchUntilExcludedEncoder name value

        MatchFor name number ->
            matchForEncoder name number

        MatchUntilEnd name ->
            matchUntilEndEncoder name


oneOfEncoder : String -> List String -> Encode.Value
oneOfEncoder name values =
    Encode.object
        [ ( "type", Encode.string "oneOf" )
        , ( "values", Encode.list Encode.string values )
        , ( "name", Encode.string name )
        ]


timeEncoder : String -> TimePattern -> Encode.Value
timeEncoder name pattern =
    Encode.object
        [ ( "type", Encode.string "time" )
        , ( "pattern", Encode.string pattern )
        , ( "name", Encode.string name )
        ]


dateEncoder : String -> DatePattern -> Encode.Value
dateEncoder name pattern =
    Encode.object
        [ ( "type", Encode.string "date" )
        , ( "pattern", Encode.string pattern )
        , ( "name", Encode.string name )
        ]


charactersEncoder : String -> String -> Encode.Value
charactersEncoder name value =
    Encode.object
        [ ( "type", Encode.string "characters" )
        , ( "value", Encode.string value )
        , ( "name", Encode.string name )
        ]


matchUntilIncludedEncoder : String -> String -> Encode.Value
matchUntilIncludedEncoder name value =
    Encode.object
        [ ( "type", Encode.string "matchUntilIncluded" )
        , ( "value", Encode.string value )
        , ( "name", Encode.string name )
        ]


matchUntilExcludedEncoder : String -> String -> Encode.Value
matchUntilExcludedEncoder name value =
    Encode.object
        [ ( "type", Encode.string "matchUntilExcluded" )
        , ( "value", Encode.string value )
        , ( "name", Encode.string name )
        ]


matchForEncoder : String -> Int -> Encode.Value
matchForEncoder name value =
    Encode.object
        [ ( "type", Encode.string "matchFor" )
        , ( "count", Encode.int value )
        , ( "name", Encode.string name )
        ]


matchUntilEndEncoder : String -> Encode.Value
matchUntilEndEncoder name =
    Encode.object
        [ ( "type", Encode.string "matchUntilEnd" )
        , ( "name", Encode.string name )
        ]


logfileParserEncoder : LogfileParser -> Encode.Value
logfileParserEncoder parser =
    Encode.object
        [ ( "name", Encode.string parser.name )
        , ( "parsers", Encode.list namedParserEncoder parser.parsers )
        ]


namedParserEncoder : ( String, ElementaryParser ) -> Encode.Value
namedParserEncoder ( name, parser ) =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "parser", elementaryParserEncoder parser )
        ]


logfileParserApplicationEncoder : LogfileParserApplicationData -> Encode.Value
logfileParserApplicationEncoder data =
    Encode.object
        [ ( "target", Encode.string data.target )
        , ( "parser", logfileParserEncoder data.parser )
        ]


parsersDataDecoder : Decoder (List ElementaryParser)
parsersDataDecoder =
    Decode.list parserDataDecoder


parserDataDecoder : Decoder ElementaryParser
parserDataDecoder =
    field "type" string
        |> Decode.andThen parserDataDecoderHelp


parserDataDecoderHelp : String -> Decoder ElementaryParser
parserDataDecoderHelp typeName =
    case typeName of
        "oneOf" ->
            oneOfParserDecoder

        "time" ->
            timeParserDecoder

        "date" ->
            dateParserDecoder

        "characters" ->
            charactersParserDecoder

        "matchUntilIncluded" ->
            matchUntilIncludedParserDecoder

        "matchUntilExcluded" ->
            matchUntilExcludedParserDecoder

        "matchFor" ->
            matchForParserDecoder

        "matchUntilEnd" ->
            matchUntilEndParserDecoder

        _ ->
            Decode.fail <|
                "Trying to decode parser but found incorrect type."
                    ++ "Type was "
                    ++ typeName
                    ++ "but expected one of "
                    ++ "[ "
                    ++ "\"oneOf\", \"time\", \"date\", \"characters\""
                    ++ ""
                    ++ " ]"


oneOfParserDecoder : Decoder ElementaryParser
oneOfParserDecoder =
    Decode.map2 OneOf (field "name" string) (field "values" (Decode.list string))


timeParserDecoder : Decoder ElementaryParser
timeParserDecoder =
    Decode.map2 Time (field "name" string) (field "pattern" string)


dateParserDecoder : Decoder ElementaryParser
dateParserDecoder =
    Decode.map2 Date (field "name" string) (field "pattern" string)


charactersParserDecoder : Decoder ElementaryParser
charactersParserDecoder =
    Decode.map2 Characters (field "name" string) (field "value" string)


matchUntilIncludedParserDecoder : Decoder ElementaryParser
matchUntilIncludedParserDecoder =
    Decode.map2 MatchUntilIncluded (field "name" string) (field "value" string)


matchUntilExcludedParserDecoder : Decoder ElementaryParser
matchUntilExcludedParserDecoder =
    Decode.map2 MatchUntilExcluded (field "name" string) (field "value" string)


matchForParserDecoder : Decoder ElementaryParser
matchForParserDecoder =
    Decode.map2 MatchFor (field "name" string) (field "count" int)


matchUntilEndParserDecoder : Decoder ElementaryParser
matchUntilEndParserDecoder =
    Decode.map MatchUntilEnd (field "name" string)


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
