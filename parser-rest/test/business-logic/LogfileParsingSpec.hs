module LogfileParsingSpec
( spec
) where


import Data.Either (isRight)
import Test.Hspec
import Data.Time (TimeOfDay (..))

import ElementaryParsing
import LogfileParsing

import qualified BusinessLogicModels as BM



spec :: Spec
spec =
    describe "applyLogfileParser" $ do
        context "when provided with a valid logfileParser" $ do
            context "parsing a loglevel and a date and the rest" $ do
                it "returns the parsed result" $ do
                    let parser = BM.LogfileParser
                                    "someName"
                                    [ BM.NamedElementaryParser "loglevel" (BM.ElementaryParser "oneOfP" (BM.ParsingOptions { BM.keepResult = True }) (BM.OneOf ["INFO", "INCIDENT", "ERROR"]))
                                    , BM.NamedElementaryParser "whitespace" (BM.ElementaryParser "whitespaceP" (BM.ParsingOptions { BM.keepResult = True }) (BM.Characters " "))
                                    , BM.NamedElementaryParser "date" (BM.ElementaryParser "dateP" (BM.ParsingOptions { BM.keepResult = True }) (BM.Date "YYYY.MM.DD"))
                                    , BM.NamedElementaryParser "whitespace" (BM.ElementaryParser "whitespaceP" (BM.ParsingOptions { BM.keepResult = False }) (BM.Characters " "))
                                    , BM.NamedElementaryParser "message" (BM.ElementaryParser "endP" (BM.ParsingOptions { BM.keepResult = True }) BM.MatchUntilEnd)
                                    ]
                    let target = "INFO 2020.12.21 some message stuff\n\
                                  \INCIDENT 2021.11.21 msg2\n\
                                  \ERROR 2022.05.13 msg3"

                    applyLogfileParser target parser `shouldBe` ( Right $ BM.LogfileParsingResult
                                                                    [ [ BM.ElementaryParsingResult "loglevel" (BM.OneOfResult "INFO")
                                                                      , BM.ElementaryParsingResult "whitespace" (BM.CharactersResult " ")
                                                                      , BM.ElementaryParsingResult "date" (BM.DateResult $ read "2020-12-21")
                                                                      , BM.ElementaryParsingResult "message" (BM.MatchUntilEndResult "some message stuff")
                                                                      ]
                                                                    , [ BM.ElementaryParsingResult "loglevel" (BM.OneOfResult "INCIDENT")
                                                                      , BM.ElementaryParsingResult "whitespace" (BM.CharactersResult " ")
                                                                      , BM.ElementaryParsingResult "date" (BM.DateResult $ read "2021-11-21")
                                                                      , BM.ElementaryParsingResult "message" (BM.MatchUntilEndResult "msg2")
                                                                      ]
                                                                    , [ BM.ElementaryParsingResult "loglevel" (BM.OneOfResult "ERROR")
                                                                      , BM.ElementaryParsingResult "whitespace" (BM.CharactersResult " ")
                                                                      , BM.ElementaryParsingResult "date" (BM.DateResult $ read "2022-05-13")
                                                                      , BM.ElementaryParsingResult "message" (BM.MatchUntilEndResult "msg3")
                                                                      ]
                                                                    ]
                                                                )


            context "parsing a time, everything until a correlation id, the id itself, then the closing id tag, 4 chars and the rest" $ do
                it "returns the parsed result" $ do
                    let parser = BM.LogfileParser
                                    "someName"
                                    [ BM.NamedElementaryParser "time" (BM.ElementaryParser "timeP" (BM.ParsingOptions { BM.keepResult = True }) (BM.Time "HH:MM"))
                                    , BM.NamedElementaryParser "whitespace" (BM.ElementaryParser "whitespaceP" (BM.ParsingOptions { BM.keepResult = True }) (BM.Characters " "))
                                    , BM.NamedElementaryParser "untilCorrelationId" (BM.ElementaryParser "untilIdP" (BM.ParsingOptions { BM.keepResult = True }) (BM.MatchUntilIncluded "<correlationId>"))
                                    , BM.NamedElementaryParser "correlationId" (BM.ElementaryParser "idP" (BM.ParsingOptions { BM.keepResult = True }) (BM.MatchUntilExcluded "</correlationId>"))
                                    , BM.NamedElementaryParser "closingIdTag" (BM.ElementaryParser "closingIdP" (BM.ParsingOptions { BM.keepResult = True }) (BM.Characters "</correlationId>"))
                                    , BM.NamedElementaryParser "whitespace" (BM.ElementaryParser "whitespaceP" (BM.ParsingOptions { BM.keepResult = False }) (BM.Characters " "))
                                    , BM.NamedElementaryParser "4Chars" (BM.ElementaryParser "4CharP" (BM.ParsingOptions { BM.keepResult = True }) (BM.MatchFor 4))
                                    , BM.NamedElementaryParser "whitespace" (BM.ElementaryParser "whitespaceP" (BM.ParsingOptions { BM.keepResult = False }) (BM.Characters " "))
                                    , BM.NamedElementaryParser "message" (BM.ElementaryParser "endP" (BM.ParsingOptions { BM.keepResult = True }) BM.MatchUntilEnd)
                                    ]
                    let target = "5:38 stuff before <correlationId>myCorrelationId1</correlationId> 4cha some message stuff\n\
                                 \9:45 prelude to the <correlationId>myCorrelationId42</correlationId> same another message till the end!\n\
                                 \14:09 prefix<correlationId>myCorrelationId15</correlationId> stuf untilTheEnd but again not into the next line"

                    applyLogfileParser target parser `shouldBe` ( Right $ BM.LogfileParsingResult
                                                                    [ [ BM.ElementaryParsingResult "time" (BM.TimeResult $ TimeOfDay 5 38 0)
                                                                      , BM.ElementaryParsingResult "whitespace" (BM.CharactersResult " ")
                                                                      , BM.ElementaryParsingResult "untilCorrelationId" (BM.MatchUntilIncludedResult "stuff before <correlationId>")
                                                                      , BM.ElementaryParsingResult "correlationId" (BM.MatchUntilExcludedResult "myCorrelationId1")
                                                                      , BM.ElementaryParsingResult "closingIdTag" (BM.CharactersResult "</correlationId>")
                                                                      , BM.ElementaryParsingResult "4Chars" (BM.MatchForResult "4cha")
                                                                      , BM.ElementaryParsingResult "message" (BM.MatchUntilEndResult "some message stuff")
                                                                      ]
                                                                    , [ BM.ElementaryParsingResult "time" (BM.TimeResult $ TimeOfDay 9 45 00)
                                                                      , BM.ElementaryParsingResult "whitespace" (BM.CharactersResult " ")
                                                                      , BM.ElementaryParsingResult "untilCorrelationId" (BM.MatchUntilIncludedResult "prelude to the <correlationId>")
                                                                      , BM.ElementaryParsingResult "correlationId" (BM.MatchUntilExcludedResult "myCorrelationId42")
                                                                      , BM.ElementaryParsingResult "closingIdTag" (BM.CharactersResult "</correlationId>")
                                                                      , BM.ElementaryParsingResult "4Chars" (BM.MatchForResult "same")
                                                                      , BM.ElementaryParsingResult "message" (BM.MatchUntilEndResult "another message till the end!")
                                                                      ]
                                                                    , [ BM.ElementaryParsingResult "time" (BM.TimeResult $ TimeOfDay 14 09 00)
                                                                      , BM.ElementaryParsingResult "whitespace" (BM.CharactersResult " ")
                                                                      , BM.ElementaryParsingResult "untilCorrelationId" (BM.MatchUntilIncludedResult "prefix<correlationId>")
                                                                      , BM.ElementaryParsingResult "correlationId" (BM.MatchUntilExcludedResult "myCorrelationId15")
                                                                      , BM.ElementaryParsingResult "closingIdTag" (BM.CharactersResult "</correlationId>")
                                                                      , BM.ElementaryParsingResult "4Chars" (BM.MatchForResult "stuf")
                                                                      , BM.ElementaryParsingResult "message" (BM.MatchUntilEndResult "untilTheEnd but again not into the next line")
                                                                      ]
                                                                    ]
                                                                )
