module Models.Shared.ElementaryParser exposing
    ( BasicParser(..)
    , ElementaryParser(..)
    , ParsingOption(..)
    , ParsingOptions
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

import Json.Decode as Decode exposing (Decoder, bool, field, int, map3, maybe, string)
import Json.Encode as Encode



-- DATA


type alias TimePattern =
    String


type alias DatePattern =
    String


type alias ParsingOptions =
    { keepResult : Bool }


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
    = ElementaryParser String ParsingOptions BasicParser



-- ENCODING


elementaryParserEncoder : ElementaryParser -> Encode.Value
elementaryParserEncoder (ElementaryParser name options parser) =
    case parser of
        OneOf values ->
            oneOfEncoder name options values

        Time pattern ->
            timeEncoder name options pattern

        Date pattern ->
            dateEncoder name options pattern

        Characters value ->
            charactersEncoder name options value

        MatchUntilIncluded value ->
            matchUntilIncludedEncoder name options value

        MatchUntilExcluded value ->
            matchUntilExcludedEncoder name options value

        MatchFor number ->
            matchForEncoder name options number

        MatchUntilEnd ->
            matchUntilEndEncoder name options



-- oneOfEncoder : String -> List String -> Encode.Value


oneOfEncoder : String -> ParsingOptions -> List String -> Encode.Value
oneOfEncoder name options values =
    Encode.object
        [ ( "type", Encode.string "oneOf" )
        , ( "values", Encode.list Encode.string values )
        , ( "name", Encode.string name )
        , ( "options", encodeParsingOptions options )
        ]


timeEncoder : String -> ParsingOptions -> TimePattern -> Encode.Value
timeEncoder name options pattern =
    Encode.object
        [ ( "type", Encode.string "time" )
        , ( "pattern", Encode.string pattern )
        , ( "name", Encode.string name )
        ]


dateEncoder : String -> ParsingOptions -> DatePattern -> Encode.Value
dateEncoder name options pattern =
    Encode.object
        [ ( "type", Encode.string "date" )
        , ( "pattern", Encode.string pattern )
        , ( "name", Encode.string name )
        ]


charactersEncoder : String -> ParsingOptions -> String -> Encode.Value
charactersEncoder name options value =
    Encode.object
        [ ( "type", Encode.string "characters" )
        , ( "value", Encode.string value )
        , ( "name", Encode.string name )
        ]


matchUntilIncludedEncoder : String -> ParsingOptions -> String -> Encode.Value
matchUntilIncludedEncoder name options value =
    Encode.object
        [ ( "type", Encode.string "matchUntilIncluded" )
        , ( "value", Encode.string value )
        , ( "name", Encode.string name )
        ]


matchUntilExcludedEncoder : String -> ParsingOptions -> String -> Encode.Value
matchUntilExcludedEncoder name options value =
    Encode.object
        [ ( "type", Encode.string "matchUntilExcluded" )
        , ( "value", Encode.string value )
        , ( "name", Encode.string name )
        ]


matchForEncoder : String -> ParsingOptions -> Int -> Encode.Value
matchForEncoder name options value =
    Encode.object
        [ ( "type", Encode.string "matchFor" )
        , ( "count", Encode.int value )
        , ( "name", Encode.string name )
        ]


matchUntilEndEncoder : String -> ParsingOptions -> Encode.Value
matchUntilEndEncoder name options =
    Encode.object
        [ ( "type", Encode.string "matchUntilEnd" )
        , ( "name", Encode.string name )
        ]


encodeParsingOptions : ParsingOptions -> Encode.Value
encodeParsingOptions options =
    Encode.object
        [ ( "keepResult", Encode.bool options.keepResult )
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
    Decode.map3 ElementaryParser (field "name" string) parsingOptionsDecoder basicParserDecoder


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


parsingOptionsDecoder : Decoder ParsingOptions
parsingOptionsDecoder =
    --Decode.succeed []
    Decode.map ParsingOptions
        (field "keepResult" bool)



-- fromMaybe : [Decoder (Maybe ParsingOption)]
-- fromMaybe
-- parsingOptionsDecoder : Decoder ParsingOptions
-- parsingOptionsDecoder =
--     Decode.succeed
--         (ParsingOptions
--             [ decodedKeepResult
--             ]
--         )
-- decodedKeepResult : ParsingOption
-- decodedKeepResult =
--     let
--         decodingResult =
--             Decode.decodeValue (field "keepResult" bool)
--     in
--     case decodingResult of
--         Ok keepResult ->
--             KeepResult keepResult
--
--         Err err ->
--             KeepResult True
-- Decode.map KeepResult (Decode.map maybeWithDefault (Decode.maybe (field "keepResult" bool)))
