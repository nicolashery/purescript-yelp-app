module App.Server.Main where

import Prelude

import Data.Either (Either(..))
import Data.Foreign (Foreign(), ForeignError(), toForeign)
import Data.Foreign.Class (read)
import Data.Function (Fn0(), mkFn0, Fn2(), mkFn2)

import App.Model
  ( SearchQuery(..)
  , ErrorResponse(..)
  , SearchResponse(..)
  , toForeignBusinesses
  )
import App.UI
  ( renderSearchPageToString
  , renderError
  , renderApiError
  , renderResults
  , renderWelcomeMessage
  )

getBusinessesFromSearch :: Foreign -> Foreign
getBusinessesFromSearch fSearchResponse =
  case read fSearchResponse :: Either ForeignError SearchResponse of
    Left err -> toForeign { error: show err }
    Right (SearchResponse { businesses = businesses }) ->
      toForeignBusinesses businesses

emptySearchQuery :: SearchQuery
emptySearchQuery = SearchQuery { term: "", location: "" }

readSearchQueryOrEmtpy :: Foreign -> SearchQuery
readSearchQueryOrEmtpy fQuery =
  case read fQuery :: Either ForeignError SearchQuery of
    Left err -> emptySearchQuery
    Right query -> query

renderSearchPageError :: Fn2 Foreign Foreign String
renderSearchPageError = mkFn2 $ \fQuery fErrorResponse ->
  let query = readSearchQueryOrEmtpy fQuery
      content = case read fErrorResponse :: Either ForeignError ErrorResponse of
        Left err -> renderError err
        Right (ErrorResponse { error = error }) -> renderApiError error
  in renderSearchPageToString query content

renderSearchPageResults :: Fn2 Foreign Foreign String
renderSearchPageResults = mkFn2 $ \fQuery fSearchResponse ->
  let query = readSearchQueryOrEmtpy fQuery
      content = case read fSearchResponse :: Either ForeignError SearchResponse of
        Left err -> renderError err
        Right (SearchResponse { businesses = results }) ->
          renderResults query results
  in renderSearchPageToString query content

renderSearchPageWelcome :: Fn0 String
renderSearchPageWelcome = mkFn0 $ \_ ->
  renderSearchPageToString emptySearchQuery renderWelcomeMessage
