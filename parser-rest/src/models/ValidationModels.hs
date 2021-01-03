{-# LANGUAGE OverloadedStrings #-}


module ValidationModels
( ValidationError (..)
, ValidationType (..)
) where

import Data.Aeson


data ValidationError = ValidationError ValidationType String

instance ToJSON ValidationError where
  toJSON (ValidationError valType reason) =
    case valType of
      FieldValidation fieldName ->
        object [ "field" .= fieldName, "reason" .= reason ]

      ExistsValidation target ->
        object [ "target" .= target, "reason" .= reason ]


data ValidationType =
    FieldValidation String
  | ExistsValidation String
