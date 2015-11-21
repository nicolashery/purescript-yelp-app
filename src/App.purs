-- Module used for interfacing with JS-land
module App where

import Prelude

import Data.Either
import Data.Foreign (Foreign(), ForeignError(), toForeign)
import Data.Foreign.Class (read)

import App.Model (SearchResponse(..), toForeignBusinesses)
import App.UI (renderPageToString, renderBusinesses)

getBusinessesFromSearch :: Foreign -> Foreign
getBusinessesFromSearch fSearchResponse =
  case read fSearchResponse :: Either ForeignError SearchResponse of
    Left err -> toForeign { error: show err }
    Right (SearchResponse { businesses = businesses }) ->
      toForeignBusinesses businesses

renderHomePageToHtml :: Foreign -> String
renderHomePageToHtml fSearchResponse =
  case read fSearchResponse :: Either ForeignError SearchResponse of
    Left err -> show err
    Right (SearchResponse { businesses = businesses }) ->
      renderPageToString (renderBusinesses businesses)
