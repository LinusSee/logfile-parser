module Models.Shared.LogfileParser exposing
    ( LogfileParser
    , LogfileParserId
    , logfileParserEncoder
    , logfileParserIdsDecoder
    )

import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Encode as Encode
import Models.Shared.ElementaryParser as ElementaryParser



-- DATA


type alias LogfileParser =
    { name : String
    , parsers : List ( String, ElementaryParser.ElementaryParser )
    }


type alias LogfileParserId =
    { parserId : String
    , name : String
    }



-- ENCODING


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



-- DECODING


logfileParserIdsDecoder : Decoder (List LogfileParserId)
logfileParserIdsDecoder =
    Decode.list logfileParserIdDecoder


logfileParserIdDecoder : Decoder LogfileParserId
logfileParserIdDecoder =
    Decode.map2 LogfileParserId (field "id" string) (field "name" string)
