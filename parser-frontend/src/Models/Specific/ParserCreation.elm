module Models.Specific.ParserCreation exposing
    ( ParserFormData
    , parserApplicationDecoder
    , parserEncoder
    )

import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Encode as Encode
import Models.Shared.ElementaryParser as ElementaryParser



-- DATA


type alias ParserFormData =
    { patternType : String
    , matching : String
    , name : String
    , parsingOptions : ElementaryParser.ParsingOptions
    }



-- ENCODING


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


toMatchingList : String -> List String
toMatchingList matching =
    let
        inputList =
            String.split "," matching

        cleanupElement element =
            String.trim element |> String.slice 1 -1
    in
    List.map cleanupElement inputList



-- DECODING


parserApplicationDecoder : Decoder ( String, String )
parserApplicationDecoder =
    Decode.map2 Tuple.pair
        (field "name" string)
        (field "result" string)
