module Models.Shared.ParserApplication exposing (..)

import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Encode as Encode
import Models.Shared.ElementaryParser as ElementaryParser
import Models.Shared.LogfileParser as LogfileParser



-- DATA


type alias ParserApplicationData =
    { target : String
    , parser : ElementaryParser.ElementaryParser
    }


type alias LogfileParserApplicationData =
    { target : String
    , parser : LogfileParser.LogfileParser
    }



-- ENCODING


logfileParserApplicationEncoder : LogfileParserApplicationData -> Encode.Value
logfileParserApplicationEncoder data =
    Encode.object
        [ ( "target", Encode.string data.target )
        , ( "parser", LogfileParser.logfileParserEncoder data.parser )
        ]



-- DECODING


logfileParserApplicationDecoder : Decoder (List (List ( String, String )))
logfileParserApplicationDecoder =
    field "result" (Decode.list (Decode.list parserApplicationDecoder))


parserApplicationDecoder : Decoder ( String, String )
parserApplicationDecoder =
    Decode.map2 Tuple.pair
        (field "name" string)
        (field "result" string)
