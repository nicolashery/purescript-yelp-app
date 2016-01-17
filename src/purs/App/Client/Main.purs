module App.Client.Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION(), Error(), error, message)
import Control.Monad.Error.Class (throwError)
import Data.DOM.Simple.Element
  ( setAttribute
  , removeAttribute
  , classAdd
  , classRemove
  , value
  , setInnerHTML
  )
import Data.DOM.Simple.Events
  ( preventDefault
  , MouseEventType(MouseClickEvent)
  , addMouseEventListener
  )
import Data.DOM.Simple.Types (DOMEvent())
import qualified Data.DOM.Simple.Window as W
import Data.Either (Either(..))
import Data.Foreign (ForeignError())
import Data.Foreign.Class (readJSON)
import DOM (DOM())
import Network.HTTP.Affjax (AJAX(), affjax, defaultRequest)
import Network.HTTP.Method (Method(GET))
import Network.HTTP.StatusCode (StatusCode(StatusCode))
import Text.Smolder.Renderer.String (render)

import App.Model
  ( SearchQuery(..)
  , ErrorResponse(..)
  , SearchResponse(..)
  , Business()
  , urlStringifySearchQuery
  , urlParseSearchQuery
  )
import App.UI (renderSpinner, renderError, renderResults)
import App.Utils (extractQueryString, unsafeQuerySelector)
import qualified App.Client.Router as Router

start :: forall eff. Eff (exception :: EXCEPTION, ajax :: AJAX, dom :: DOM | eff) Unit
start = do
  attachEventListeners
  Router.start

attachEventListeners :: forall eff. Eff (exception :: EXCEPTION, ajax :: AJAX, dom :: DOM | eff) Unit
attachEventListeners = do
  doc <- W.document W.globalWindow
  button <- unsafeQuerySelector ".search-button" doc
  addMouseEventListener MouseClickEvent handleClickSearch button
  Router.addUrlChangeListener handleUrlChange

handleClickSearch :: forall eff. DOMEvent -> Eff (exception :: EXCEPTION, dom :: DOM | eff) Unit
handleClickSearch e = do
  preventDefault e
  query <- getFormValues
  let qs = urlStringifySearchQuery query
  Router.navigate ("/" ++ qs)

handleUrlChange :: forall eff. String -> Eff (exception :: EXCEPTION, ajax :: AJAX, dom :: DOM | eff) Unit
handleUrlChange url = do
  disableForm
  showSpinner
  let query = urlParseSearchQuery (extractQueryString url)
  setFormValues query
  runAff handleSearchError (handleSearchSuccess query) (searchBusinesses query)

getFormValues :: forall eff. Eff (exception :: EXCEPTION, dom:: DOM | eff) SearchQuery
getFormValues = do
  doc <- W.document W.globalWindow
  termInput <- unsafeQuerySelector "input[name=term]" doc
  term <- value termInput
  locationInput <- unsafeQuerySelector "input[name=location]" doc
  location <- value locationInput
  return (SearchQuery { term: term, location: location })

setFormValues :: forall eff. SearchQuery -> Eff (exception :: EXCEPTION, dom :: DOM | eff) Unit
setFormValues (SearchQuery { term, location }) = do
  doc <- W.document W.globalWindow
  termInput <- unsafeQuerySelector "input[name=term]" doc
  setAttribute "value" term termInput
  locationInput <- unsafeQuerySelector "input[name=location]" doc
  setAttribute "value" location locationInput

disableForm :: forall eff. Eff (exception :: EXCEPTION, dom :: DOM | eff) Unit
disableForm = do
  doc <- W.document W.globalWindow
  button <- unsafeQuerySelector ".search-button" doc
  setAttribute "disabled" "" button
  classAdd "disabled" button

enableForm :: forall eff. Eff (exception :: EXCEPTION, dom :: DOM | eff) Unit
enableForm = do
  doc <- W.document W.globalWindow
  button <- unsafeQuerySelector ".search-button" doc
  removeAttribute "disabled" button
  classRemove "disabled" button

showSpinner :: forall eff. Eff (exception :: EXCEPTION, dom :: DOM | eff) Unit
showSpinner = do
  doc <- W.document W.globalWindow
  content <- unsafeQuerySelector ".content" doc
  setInnerHTML (render $ renderSpinner) content

searchBusinesses :: forall eff. SearchQuery -> Aff (ajax :: AJAX | eff) (Array Business)
searchBusinesses query = do
  let qs = urlStringifySearchQuery query
  let url = "/api/search" ++ qs
  res <- affjax $ defaultRequest { url = url, method = GET }
  if res.status /= StatusCode 200
    then case readJSON res.response :: Either ForeignError ErrorResponse of
      Left err -> throwError (error (show err))
      Right (ErrorResponse { error = err }) -> throwError (error (show err))
    else case readJSON res.response :: Either ForeignError SearchResponse of
      Left err -> throwError (error (show err))
      Right (SearchResponse { businesses = businesses }) -> return businesses

handleSearchError :: forall eff. Error -> Eff (exception :: EXCEPTION, dom :: DOM | eff) Unit
handleSearchError err = do
  showError (message err)
  enableForm

handleSearchSuccess :: forall eff. SearchQuery -> Array Business -> Eff (exception :: EXCEPTION, dom :: DOM | eff) Unit
handleSearchSuccess query businesses = do
  showResults query businesses
  enableForm

showError :: forall a eff. (Show a) => a -> Eff (exception :: EXCEPTION, dom :: DOM | eff) Unit
showError err = do
  doc <- W.document W.globalWindow
  content <- unsafeQuerySelector ".content" doc
  setInnerHTML (render $ renderError err) content

showResults :: forall eff. SearchQuery -> Array Business -> Eff (exception :: EXCEPTION, dom :: DOM | eff) Unit
showResults query results = do
  doc <- W.document W.globalWindow
  content <- unsafeQuerySelector ".content" doc
  setInnerHTML (render $ renderResults query results) content

-- Dummy function because psc-bundle needs to call something
main :: String -> String
main _ = "noop"
