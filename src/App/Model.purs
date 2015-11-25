module App.Model where

import Prelude

import Data.Foreign (Foreign())
import Data.Foreign.Class (IsForeign)
import Data.Foreign.Generic
  ( Options()
  , defaultOptions
  , readGeneric
  , toForeignGeneric
  )
import Data.Generic (Generic, gShow)
import Data.Maybe (Maybe())
import Data.Tuple (Tuple())


opts :: Options
opts = defaultOptions { unwrapNewtypes = true, tupleAsArray = true }

-- Types
-------------------------------------------------------------------------------

type BusinessId = String

newtype Business = Business {
  id :: BusinessId,
  name :: String,
  categories :: Maybe (Array (Tuple String String))
}

newtype SearchQuery = SearchQuery {
  term :: String,
  location :: String
}

newtype ApiError = ApiError {
  name :: String,
  message :: String
}

newtype ErrorResponse = ErrorResponse {
  error :: ApiError
}

newtype SearchResponse = SearchResponse {
  businesses :: Array Business
}

-- Functions
-------------------------------------------------------------------------------

toForeignBusinesses :: Array Business -> Foreign
toForeignBusinesses businesses = toForeignGeneric opts businesses

-- Instances
-------------------------------------------------------------------------------

-- Business

derive instance genericBusiness :: Generic Business

instance showBusiness :: Show Business where
  show = gShow

instance businessIsForeign :: IsForeign Business where
  read = readGeneric opts

-- SearchQuery

derive instance genericSearchQuery :: Generic SearchQuery

instance showSearchQuery :: Show SearchQuery where
  show = gShow

instance searchQueryIsForeign :: IsForeign SearchQuery where
  read = readGeneric opts

-- ApiError

derive instance genericApiError :: Generic ApiError

instance showApiError :: Show ApiError where
  show = gShow

instance apiErrorIsForeign :: IsForeign ApiError where
  read = readGeneric opts

-- ErrorResponse

derive instance genericErrorResponse :: Generic ErrorResponse

instance showErrorResponse :: Show ErrorResponse where
  show = gShow

instance errorResponseIsForeign :: IsForeign ErrorResponse where
  read = readGeneric opts

-- SearchResponse

derive instance genericSearchResponse :: Generic SearchResponse

instance showSearchResponse :: Show SearchResponse where
  show = gShow

instance searchResponseIsForeign :: IsForeign SearchResponse where
  read = readGeneric opts
