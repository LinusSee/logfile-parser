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
    = OneOf (List String)
    | Time TimePattern
    | Date DatePattern
    | Characters String


type alias ParserFormData =
    { patternType : String
    , matching : String
    , name : String
    }


type alias SampleData =
    { val1 : Int
    , val2 : String
    , val3 : String
    }



-- HTTP


sampleDataDecoder : Decoder SampleData
sampleDataDecoder =
    map3 SampleData
        (field "dummy1" int)
        (field "dummy2" string)
        (field "dummy3" string)



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
                , ( "values", Encode.list Encode.string (toMatchingList formData.matching) )
                ]

        "time" ->
            Encode.object
                [ ( "type", Encode.string "time" )
                , ( "pattern", Encode.string formData.matching )
                ]

        "date" ->
            Encode.object
                [ ( "type", Encode.string "date" )
                , ( "pattern", Encode.string formData.matching )
                ]

        "characters" ->
            Encode.object
                [ ( "type", Encode.string "characters" )
                , ( "value", Encode.string formData.matching )
                ]

        -- TEMP: Need to find out how to solve this
        _ ->
            Encode.object
                [ ( "type", Encode.string "invalidType" )
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
    Decode.map OneOf (field "values" (Decode.list string))


timeParserDecoder : Decoder ElementaryParser
timeParserDecoder =
    Decode.map Time (field "pattern" string)


charactersParserDecoder : Decoder ElementaryParser
charactersParserDecoder =
    Decode.map Characters (field "value" string)


dateParserDecoder : Decoder ElementaryParser
dateParserDecoder =
    Decode.map Date (field "pattern" string)
