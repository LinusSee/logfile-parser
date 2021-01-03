{-# LANGUAGE OverloadedStrings #-}


module HttpErrors
( Problem (..)
) where

import Data.Aeson
import ValidationModels (ValidationError)


data Problem = Problem
  { problemType :: String
  , title :: String
  , status :: Int
  , detail :: String
  , errors :: [ValidationError]
  }

instance ToJSON Problem where
  toJSON problem = object
      [ "type" .= problemType problem
      , "title" .= title problem
      , "status" .= status problem
      , "detail" .= detail problem
      , "errors" .= errors problem
      ]
