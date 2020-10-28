module Page.ParserCreation exposing (..)

import DecEnc
import Http
import Session exposing (Session)



-- MODEL


type Model
    = CreateParser Session CreateParserModel


type alias CreateParserModel =
    { requestState : HttpRequestState
    }


type HttpRequestState
    = Failure
    | Loading
    | Success DecEnc.ParserFormData DecEnc.SampleData (List DecEnc.ElementaryParser)



-- UPDATE


type Msg
    = GotDummyData (Result Http.Error DecEnc.SampleData)
    | GotElementaryParsers (Result Http.Error (List DecEnc.ElementaryParser))
    | PostedParser (Result Http.Error ())
    | ChangeForm FormChanged String
    | Reset
    | Submit DecEnc.ParserFormData


type FormChanged
    = ChangePatternType
    | ChangeMatching
    | ChangeName
