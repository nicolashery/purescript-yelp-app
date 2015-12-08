module App.Client.Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (Error(), error, message)
import Control.Monad.Error.Class (throwError)
import Data.DOM.Simple.Element
  ( querySelector
  , setStyleAttr
  , setAttribute
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
import Data.Maybe (Maybe(..))
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
  )
import App.UI (renderSpinner, renderError, renderResults)
import App.Utils (encodeURIComponent)

start :: forall eff. Eff (dom :: DOM | eff) Unit
start = do
  attachEventListeners

attachEventListeners :: forall eff. Eff (dom :: DOM | eff) Unit
attachEventListeners = do
  doc <- W.document W.globalWindow
  Just button <- querySelector ".search-button" doc
  addMouseEventListener MouseClickEvent handleClickSearch button

handleClickSearch :: forall eff. DOMEvent -> Eff (ajax :: AJAX, dom :: DOM | eff) Unit
handleClickSearch e = do
  preventDefault e
  disableForm
  showSpinner
  query <- getFormValues
  runAff handleSearchError (handleSearchSuccess query) (search query)

getFormValues :: forall eff. Eff (dom:: DOM | eff) SearchQuery
getFormValues = do
  doc <- W.document W.globalWindow
  Just termInput <- querySelector "input[name=term]" doc
  term <- value termInput
  Just locationInput <- querySelector "input[name=location]" doc
  location <- value locationInput
  return (SearchQuery { term: term, location: location })

disableForm :: forall eff. Eff (dom:: DOM | eff) Unit
disableForm = do
  doc <- W.document W.globalWindow
  Just button <- querySelector ".search-button" doc
  setAttribute "disabled" "" button
  classAdd "disabled" button

enableForm :: forall eff. Eff (dom:: DOM | eff) Unit
enableForm = do
  doc <- W.document W.globalWindow
  Just button <- querySelector ".search-button" doc
  removeAttribute "disabled" button
  classRemove "disabled" button

showSpinner :: forall eff. Eff (dom:: DOM | eff) Unit
showSpinner = do
  doc <- W.document W.globalWindow
  Just content <- querySelector ".content" doc
  setInnerHTML (render $ renderSpinner) content

search :: forall eff. SearchQuery -> Aff (ajax :: AJAX | eff) (Array Business)
search (SearchQuery { term = term, location = location }) = do
  let qs = "?term=" ++ encodeURIComponent term ++ "&location=" ++ encodeURIComponent location
  let url = "/api/search" ++ qs
  res <- affjax $ defaultRequest { url = url, method = GET }
  if res.status /= StatusCode 200
    then case readJSON res.response :: Either ForeignError ErrorResponse of
      Left err -> throwError (error (show err))
      Right (ErrorResponse { error = err }) -> throwError (error (show err))
    else case readJSON res.response :: Either ForeignError SearchResponse of
      Left err -> throwError (error (show err))
      Right (SearchResponse { businesses = businesses }) -> return businesses

handleSearchError :: forall eff. Error -> Eff (dom :: DOM | eff) Unit
handleSearchError err = do
  showError (message err)
  enableForm

handleSearchSuccess :: forall eff. SearchQuery -> Array Business -> Eff (dom :: DOM | eff) Unit
handleSearchSuccess query businesses = do
  showResults query businesses
  enableForm

showError :: forall a eff. (Show a) => a -> Eff (dom :: DOM | eff) Unit
showError err = do
  doc <- W.document W.globalWindow
  Just content <- querySelector ".content" doc
  setInnerHTML (render $ renderError err) content

showResults :: forall eff. SearchQuery -> Array Business -> Eff (dom :: DOM | eff) Unit
showResults query results = do
  doc <- W.document W.globalWindow
  Just content <- querySelector ".content" doc
  setInnerHTML (render $ renderResults query results) content

-- Dummy function because psc-bundle needs to call something
main :: String -> String
main _ = "noop"
