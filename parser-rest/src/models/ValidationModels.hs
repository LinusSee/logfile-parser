{-# LANGUAGE OverloadedStrings #-}


module ValidationModels
( ValidationError (..)
, ValidationType (..)
) where

import Data.Aeson


data ValidationError = ValidationError ValidationType String
  deriving (Show, Eq)

instance ToJSON ValidationError where
  toJSON (ValidationError valType reason) =
    case valType of
      FieldValidation fieldName ->
        object [ "field" .= fieldName, "reason" .= reason ]

      QueryParamValidation paramName ->
        object [ "queryParam" .= paramName, "reason" .= reason ]

      ExistsValidation target ->
        object [ "target" .= target, "reason" .= reason ]


data ValidationType =
    FieldValidation String
  | QueryParamValidation String
  | ExistsValidation String
  deriving (Show, Eq)
