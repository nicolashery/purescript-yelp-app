-- Module used for interfacing with JS-land
module App where

import Prelude

import Data.Either (Either(..))
import Data.Foreign (Foreign(), ForeignError(), toForeign)
import Data.Foreign.Class (read)

import App.Model (ErrorResponse(..), SearchResponse(..), toForeignBusinesses)
import App.UI (renderPageToString, renderApiError, renderBusinesses)

getBusinessesFromSearch :: Foreign -> Foreign
getBusinessesFromSearch fSearchResponse =
  case read fSearchResponse :: Either ForeignError SearchResponse of
    Left err -> toForeign { error: show err }
    Right (SearchResponse { businesses = businesses }) ->
      toForeignBusinesses businesses

renderErrorPageToHtml :: Foreign -> String
renderErrorPageToHtml fErrorResponse =
  case read fErrorResponse :: Either ForeignError ErrorResponse of
    Left err -> show err
    Right (ErrorResponse { error = error }) ->
      renderPageToString (renderApiError error)

renderHomePageToHtml :: Foreign -> String
renderHomePageToHtml fSearchResponse =
  case read fSearchResponse :: Either ForeignError SearchResponse of
    Left err -> show err
    Right (SearchResponse { businesses = businesses }) ->
      renderPageToString (renderBusinesses businesses)
