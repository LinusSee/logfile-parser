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


type alias ParserFormData =
    { patternType : String
    , matching : String
    , name : String
    }



-- HTTP
-- Maybe TEMP


toMatchingList : String -> List String
toMatchingList matching =
    [ "HARDCODED", "DATA" ]


parserEncoder : ParserFormData -> Encode.Value
parserEncoder formData =
    case formData.patternType of
        "oneOf" ->
            Encode.object
                [ ( "type", Encode.string "oneOf" )
                , ( "name", Encode.string formData.name )
                , ( "values", Encode.list Encode.string (toMatchingList formData.matching) )
                ]

        "time" ->
            Encode.object
                [ ( "type", Encode.string "time" )
                , ( "name", Encode.string formData.name )
                , ( "pattern", Encode.string formData.matching )
                ]

        "date" ->
            Encode.object
                [ ( "type", Encode.string "date" )
                , ( "name", Encode.string formData.name )
                , ( "pattern", Encode.string formData.matching )
                ]

        "characters" ->
            Encode.object
                [ ( "type", Encode.string "characters" )
                , ( "name", Encode.string formData.name )
                , ( "value", Encode.string formData.matching )
                ]

        -- TEMP: Need to find out how to solve this
        _ ->
            Encode.object
                [ ( "type", Encode.string "invalidType" )
                , ( "name", Encode.string "invalidName" )
                , ( "value", Encode.string "invalidValue" )
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

        _ ->
            Decode.fail <|
                "Trying to decode parser but found incorrect type."
                    ++ "Type was "
                    ++ typeName
                    ++ "but expected one of "
                    ++ "[ \"oneOf\", \"time\", \"date\", \"characters\" ]"


oneOfParserDecoder : Decoder ElementaryParser
oneOfParserDecoder =
    Decode.map2 OneOf (field "name" string) (field "values" (Decode.list string))


timeParserDecoder : Decoder ElementaryParser
timeParserDecoder =
    Decode.map2 Time (field "name" string) (field "pattern" string)


charactersParserDecoder : Decoder ElementaryParser
charactersParserDecoder =
    Decode.map2 Characters (field "name" string) (field "value" string)


dateParserDecoder : Decoder ElementaryParser
dateParserDecoder =
    Decode.map2 Date (field "name" string) (field "pattern" string)
