module LogfileParsingSpec
( spec
) where


import Data.Either (isRight)
import Test.Hspec

import CustomParsers
import ElementaryParsing
import LogfileParsing



spec :: Spec
spec =
    describe "applyLogfileParser" $ do
        context "when provided with a valid logfileParser" $ do
            context "parsing a loglevel and a date and the rest" $ do
                it "returns the parsed result" $ do
                    let parser = LogfileParser
                                    "someName"
                                    [ ("loglevel", OneOf "oneOfP" ["INFO", "INCIDENT", "ERROR"])
                                    , ("whitespace", Characters "whitespaceP" " ")
                                    , ("date", Date "dateP" "YYYY.MM.DD")
                                    , ("whitespace", Characters "whitespaceP" " ")
                                    , ("message", MatchUntilEnd "endP")
                                    ]
                    let target = "INFO 2020.12.21 some message stuff\n\
                                  \INCIDENT 2021.11.21 msg2\n\
                                  \ERROR 2022.05.13 msg3"

                    applyLogfileParser target parser `shouldBe` ( Right $ LogfileParsingResponse
                                                                    [ [ ParsingResponse "loglevel" (OneOfResult "INFO")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "date" (DateResult "2020-12-21")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "message" (MatchUntilEndResult "some message stuff")
                                                                      ]
                                                                    , [ ParsingResponse "loglevel" (OneOfResult "INCIDENT")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "date" (DateResult "2021-11-21")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "message" (MatchUntilEndResult "msg2")
                                                                      ]
                                                                    , [ ParsingResponse "loglevel" (OneOfResult "ERROR")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "date" (DateResult "2022-05-13")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "message" (MatchUntilEndResult "msg3")
                                                                      ]
                                                                    ]
                                                                )


            context "parsing a time, everything until a correlation id, the id itself, then the closing id tag, 4 chars and the rest" $ do
                it "returns the parsed result" $ do
                    let parser = LogfileParser
                                    "someName"
                                    [ ("time", Time "timeP" "HH:MM")
                                    , ("whitespace", Characters "whitespaceP" " ")
                                    , ("untilCorrelationId", MatchUntilIncluded "untilIdP" "<correlationId>")
                                    , ("correlationId", MatchUntilExcluded "idP" "</correlationId>")
                                    , ("closingIdTag", Characters "closingIdP" "</correlationId>")
                                    , ("whitespace", Characters "whitespaceP" " ")
                                    , ("4Chars", MatchFor "4CharP" 4)
                                    , ("whitespace", Characters "whitespaceP" " ")
                                    , ("message", MatchUntilEnd "endP")
                                    ]
                    let target = "5:38 stuff before <correlationId>myCorrelationId1</correlationId> 4cha some message stuff\n\
                                 \9:45 prelude to the <correlationId>myCorrelationId42</correlationId> same another message till the end!\n\
                                 \14:09 prefix<correlationId>myCorrelationId15</correlationId> stuf untilTheEnd but again not into the next line"

                    applyLogfileParser target parser `shouldBe` ( Right $ LogfileParsingResponse
                                                                    [ [ ParsingResponse "time" (TimeResult "05:38:00")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "untilCorrelationId" (MatchUntilIncludedResult "stuff before <correlationId>")
                                                                      , ParsingResponse "correlationId" (MatchUntilExcludedResult "myCorrelationId1")
                                                                      , ParsingResponse "closingIdTag" (CharactersResult "</correlationId>")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "4Chars" (MatchForResult "4cha")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "message" (MatchUntilEndResult "some message stuff")
                                                                      ]
                                                                    , [ ParsingResponse "time" (TimeResult "09:45:00")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "untilCorrelationId" (MatchUntilIncludedResult "prelude to the <correlationId>")
                                                                      , ParsingResponse "correlationId" (MatchUntilExcludedResult "myCorrelationId42")
                                                                      , ParsingResponse "closingIdTag" (CharactersResult "</correlationId>")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "4Chars" (MatchForResult "same")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "message" (MatchUntilEndResult "another message till the end!")
                                                                      ]
                                                                    , [ ParsingResponse "time" (TimeResult "14:09:00")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "untilCorrelationId" (MatchUntilIncludedResult "prefix<correlationId>")
                                                                      , ParsingResponse "correlationId" (MatchUntilExcludedResult "myCorrelationId15")
                                                                      , ParsingResponse "closingIdTag" (CharactersResult "</correlationId>")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "4Chars" (MatchForResult "stuf")
                                                                      , ParsingResponse "whitespace" (CharactersResult " ")
                                                                      , ParsingResponse "message" (MatchUntilEndResult "untilTheEnd but again not into the next line")
                                                                      ]
                                                                    ]
                                                                )
