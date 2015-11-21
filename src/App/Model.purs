module App.Model
  ( BusinessId()
  , Business(..)
  , SearchResponse(..)
  ) where

import Prelude

import Data.Generic (Generic, gShow)
import Data.Foreign.Class (IsForeign, readProp)
import Data.Foreign.Generic
  ( Options()
  , defaultOptions
  , readGeneric
  )


opts :: Options
opts = defaultOptions { unwrapNewtypes = true }

-- Types
-------------------------------------------------------------------------------

type BusinessId = String

newtype Business = Business {
  id :: BusinessId,
  name :: String
}

newtype SearchResponse = SearchResponse {
  businesses :: Array Business
}

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
