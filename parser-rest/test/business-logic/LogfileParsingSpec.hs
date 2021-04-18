module LogfileParsingSpec
( spec
) where


import Data.Either (isRight)
import Test.Hspec
import Data.Time (TimeOfDay (..))

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
                                    [ NamedElementaryParser "loglevel" (ElementaryParser "oneOfP" (ParsingOptions [KeepResult True]) (OneOf ["INFO", "INCIDENT", "ERROR"]))
                                    , NamedElementaryParser "whitespace" (ElementaryParser "whitespaceP" (ParsingOptions [KeepResult True]) (Characters " "))
                                    , NamedElementaryParser "date" (ElementaryParser "dateP" (ParsingOptions [KeepResult True]) (Date "YYYY.MM.DD"))
                                    , NamedElementaryParser "whitespace" (ElementaryParser "whitespaceP" (ParsingOptions [KeepResult False]) (Characters " "))
                                    , NamedElementaryParser "message" (ElementaryParser "endP" (ParsingOptions [KeepResult True]) MatchUntilEnd)
                                    ]
                    let target = "INFO 2020.12.21 some message stuff\n\
                                  \INCIDENT 2021.11.21 msg2\n\
                                  \ERROR 2022.05.13 msg3"

                    applyLogfileParser target parser `shouldBe` ( Right $ LogfileParsingSuccess
                                                                    [ [ NamedParsingResult "loglevel" (OneOfResult "INFO")
                                                                      , NamedParsingResult "whitespace" (CharactersResult " ")
                                                                      , NamedParsingResult "date" (DateResult $ read "2020-12-21")
                                                                      , NamedParsingResult "message" (MatchUntilEndResult "some message stuff")
                                                                      ]
                                                                    , [ NamedParsingResult "loglevel" (OneOfResult "INCIDENT")
                                                                      , NamedParsingResult "whitespace" (CharactersResult " ")
                                                                      , NamedParsingResult "date" (DateResult $ read "2021-11-21")
                                                                      , NamedParsingResult "message" (MatchUntilEndResult "msg2")
                                                                      ]
                                                                    , [ NamedParsingResult "loglevel" (OneOfResult "ERROR")
                                                                      , NamedParsingResult "whitespace" (CharactersResult " ")
                                                                      , NamedParsingResult "date" (DateResult $ read "2022-05-13")
                                                                      , NamedParsingResult "message" (MatchUntilEndResult "msg3")
                                                                      ]
                                                                    ]
                                                                )


            context "parsing a time, everything until a correlation id, the id itself, then the closing id tag, 4 chars and the rest" $ do
                it "returns the parsed result" $ do
                    let parser = LogfileParser
                                    "someName"
                                    [ NamedElementaryParser "time" (ElementaryParser "timeP" (ParsingOptions [KeepResult True]) (Time "HH:MM"))
                                    , NamedElementaryParser "whitespace" (ElementaryParser "whitespaceP" (ParsingOptions [KeepResult True]) (Characters " "))
                                    , NamedElementaryParser "untilCorrelationId" (ElementaryParser "untilIdP" (ParsingOptions [KeepResult True]) (MatchUntilIncluded "<correlationId>"))
                                    , NamedElementaryParser "correlationId" (ElementaryParser "idP" (ParsingOptions [KeepResult True]) (MatchUntilExcluded "</correlationId>"))
                                    , NamedElementaryParser "closingIdTag" (ElementaryParser "closingIdP" (ParsingOptions [KeepResult True]) (Characters "</correlationId>"))
                                    , NamedElementaryParser "whitespace" (ElementaryParser "whitespaceP" (ParsingOptions [KeepResult False]) (Characters " "))
                                    , NamedElementaryParser "4Chars" (ElementaryParser "4CharP" (ParsingOptions [KeepResult True]) (MatchFor 4))
                                    , NamedElementaryParser "whitespace" (ElementaryParser "whitespaceP" (ParsingOptions [KeepResult False]) (Characters " "))
                                    , NamedElementaryParser "message" (ElementaryParser "endP" (ParsingOptions [KeepResult True]) MatchUntilEnd)
                                    ]
                    let target = "5:38 stuff before <correlationId>myCorrelationId1</correlationId> 4cha some message stuff\n\
                                 \9:45 prelude to the <correlationId>myCorrelationId42</correlationId> same another message till the end!\n\
                                 \14:09 prefix<correlationId>myCorrelationId15</correlationId> stuf untilTheEnd but again not into the next line"

                    applyLogfileParser target parser `shouldBe` ( Right $ LogfileParsingSuccess
                                                                    [ [ NamedParsingResult "time" (TimeResult $ TimeOfDay 5 38 0)
                                                                      , NamedParsingResult "whitespace" (CharactersResult " ")
                                                                      , NamedParsingResult "untilCorrelationId" (MatchUntilIncludedResult "stuff before <correlationId>")
                                                                      , NamedParsingResult "correlationId" (MatchUntilExcludedResult "myCorrelationId1")
                                                                      , NamedParsingResult "closingIdTag" (CharactersResult "</correlationId>")
                                                                      , NamedParsingResult "4Chars" (MatchForResult "4cha")
                                                                      , NamedParsingResult "message" (MatchUntilEndResult "some message stuff")
                                                                      ]
                                                                    , [ NamedParsingResult "time" (TimeResult $ TimeOfDay 9 45 00)
                                                                      , NamedParsingResult "whitespace" (CharactersResult " ")
                                                                      , NamedParsingResult "untilCorrelationId" (MatchUntilIncludedResult "prelude to the <correlationId>")
                                                                      , NamedParsingResult "correlationId" (MatchUntilExcludedResult "myCorrelationId42")
                                                                      , NamedParsingResult "closingIdTag" (CharactersResult "</correlationId>")
                                                                      , NamedParsingResult "4Chars" (MatchForResult "same")
                                                                      , NamedParsingResult "message" (MatchUntilEndResult "another message till the end!")
                                                                      ]
                                                                    , [ NamedParsingResult "time" (TimeResult $ TimeOfDay 14 09 00)
                                                                      , NamedParsingResult "whitespace" (CharactersResult " ")
                                                                      , NamedParsingResult "untilCorrelationId" (MatchUntilIncludedResult "prefix<correlationId>")
                                                                      , NamedParsingResult "correlationId" (MatchUntilExcludedResult "myCorrelationId15")
                                                                      , NamedParsingResult "closingIdTag" (CharactersResult "</correlationId>")
                                                                      , NamedParsingResult "4Chars" (MatchForResult "stuf")
                                                                      , NamedParsingResult "message" (MatchUntilEndResult "untilTheEnd but again not into the next line")
                                                                      ]
                                                                    ]
                                                                )
