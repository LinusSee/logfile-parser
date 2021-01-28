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
        context "when provided with a valid logfileParser parsing a loglevel and a date and the rest" $ do
            it "returns the parsed result" $ do
                let parser = LogfileParser
                                "someName"
                                [ ("loglevel", OneOf "oneOfP" ["INFO", "INCIDENT", "ERROR"])
                                , ("whitespace", Characters "whitespaceP" " ")
                                , ("date", Date "dateP" "YYYY.MM.DD")
                                , ("whitespace", Characters "whitespaceP" " ")
                                , ("message", MatchUntilEnd "endP")
                                ]
                let target = "INFO 2020.12.21 some message stuff\nINCIDENT 2021.11.21 msg2\nERROR 2022.05.13 msg3"

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
