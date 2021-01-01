module Models.Shared.LogfileParser exposing
    ( LogfileParser
    , logfileParserEncoder
    )

import Json.Encode as Encode
import Models.Shared.ElementaryParser as ElementaryParser



-- DATA


type alias LogfileParser =
    { name : String
    , parsers : List ( String, ElementaryParser.ElementaryParser )
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
