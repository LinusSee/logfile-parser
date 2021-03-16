module Models.Shared.ElementaryParser exposing
    ( BasicParser(..)
    , ElementaryParser(..)
    , charactersEncoder
    , dateEncoder
    , elementaryParserEncoder
    , matchForEncoder
    , matchUntilEndEncoder
    , matchUntilExcludedEncoder
    , matchUntilIncludedEncoder
    , oneOfEncoder
    , parserDataDecoder
    , parsersDataDecoder
    , timeEncoder
    )

import Json.Decode as Decode exposing (Decoder, field, int, map3, string)
import Json.Encode as Encode



-- DATA


type alias TimePattern =
    String


type alias DatePattern =
    String


type ParsingOptions
    = ParsingOptions (List ParsingOption)


type ParsingOption
    = KeepResult Bool


type BasicParser
    = OneOf (List String)
    | Time TimePattern
    | Date DatePattern
    | Characters String
    | MatchUntilIncluded String
    | MatchUntilExcluded String
    | MatchFor Int
    | MatchUntilEnd


type ElementaryParser
    = ElementaryParser String BasicParser



-- ENCODING


elementaryParserEncoder : ElementaryParser -> Encode.Value
elementaryParserEncoder (ElementaryParser name parser) =
    case parser of
        OneOf values ->
            oneOfEncoder name values

        Time pattern ->
            timeEncoder name pattern

        Date pattern ->
            dateEncoder name pattern

        Characters value ->
            charactersEncoder name value

        MatchUntilIncluded value ->
            matchUntilIncludedEncoder name value

        MatchUntilExcluded value ->
            matchUntilExcludedEncoder name value

        MatchFor number ->
            matchForEncoder name number

        MatchUntilEnd ->
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



-- DECODING


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
            decodeElementaryParser oneOfParserDecoder

        "time" ->
            decodeElementaryParser timeParserDecoder

        "date" ->
            decodeElementaryParser dateParserDecoder

        "characters" ->
            decodeElementaryParser charactersParserDecoder

        "matchUntilIncluded" ->
            decodeElementaryParser matchUntilIncludedParserDecoder

        "matchUntilExcluded" ->
            decodeElementaryParser matchUntilExcludedParserDecoder

        "matchFor" ->
            decodeElementaryParser matchForParserDecoder

        "matchUntilEnd" ->
            decodeElementaryParser matchUntilEndParserDecoder

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


decodeElementaryParser : Decoder BasicParser -> Decoder ElementaryParser
decodeElementaryParser basicParserDecoder =
    Decode.map2 ElementaryParser (field "name" string) basicParserDecoder


oneOfParserDecoder : Decoder BasicParser
oneOfParserDecoder =
    Decode.map OneOf (field "values" (Decode.list string))


timeParserDecoder : Decoder BasicParser
timeParserDecoder =
    Decode.map Time (field "pattern" string)


dateParserDecoder : Decoder BasicParser
dateParserDecoder =
    Decode.map Date (field "pattern" string)


charactersParserDecoder : Decoder BasicParser
charactersParserDecoder =
    Decode.map Characters (field "value" string)


matchUntilIncludedParserDecoder : Decoder BasicParser
matchUntilIncludedParserDecoder =
    Decode.map MatchUntilIncluded (field "value" string)


matchUntilExcludedParserDecoder : Decoder BasicParser
matchUntilExcludedParserDecoder =
    Decode.map MatchUntilExcluded (field "value" string)


matchForParserDecoder : Decoder BasicParser
matchForParserDecoder =
    Decode.map MatchFor (field "count" int)


matchUntilEndParserDecoder : Decoder BasicParser
matchUntilEndParserDecoder =
    Decode.succeed MatchUntilEnd
