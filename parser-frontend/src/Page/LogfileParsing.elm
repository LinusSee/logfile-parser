module Page.LogfileParsing exposing (..)

import Html exposing (Html, div, text)



-- UPDATE


type Msg
    = NothingYet



-- VIEW


viewParseLogfile : Html Msg
viewParseLogfile =
    div []
        [ text "Success loading 'ParseLogfile'!"
        ]
