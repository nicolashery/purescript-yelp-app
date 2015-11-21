module App.Model
  ( BusinessId()
  , Business(..)
  , SearchResponse(..)
  , toForeignBusinesses
  ) where

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

newtype SearchResponse = SearchResponse {
  businesses :: Array Business
}

-- Functions
-------------------------------------------------------------------------------

toForeignBusinesses :: Array Business -> Foreign
toForeignBusinesses businesses = toForeignGeneric opts businesses

-- Instances
-------------------------------------------------------------------------------

derive instance genericBusiness :: Generic Business

instance showBusiness :: Show Business where
  show = gShow

instance businessIsForeign :: IsForeign Business where
  read = readGeneric defaultOptions

derive instance genericSearchResponse :: Generic SearchResponse

instance showSearchResponse :: Show SearchResponse where
  show = gShow

instance searchResponseIsForeign :: IsForeign SearchResponse where
  read = readGeneric opts
