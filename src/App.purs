-- Module used for interfacing with JS-land
module App where

import Prelude

import Data.Either
import Data.Foreign (Foreign(), ForeignError(), toForeign)
import Data.Foreign.Class (read)

import App.Model (SearchResponse(..))
import App.UI (renderPageToString, renderBusinesses)

getBusinessesFromSearch :: Foreign -> Foreign
getBusinessesFromSearch fSearchResponse =
  case read fSearchResponse :: Either ForeignError SearchResponse of
    Left _ -> toForeign []
    Right (SearchResponse { businesses = businesses }) -> toForeign businesses

renderHomePageToHtml :: Foreign -> String
renderHomePageToHtml fSearchResponse =
  case read fSearchResponse :: Either ForeignError SearchResponse of
    Left _ -> ""
    Right (SearchResponse { businesses = businesses }) ->
      renderPageToString (renderBusinesses businesses)
