module App.Client.Main where

import Prelude

import Control.Monad.Eff (Eff())
import Data.DOM.Simple.Element (querySelector, setStyleAttr, setAttribute, classAdd)
import Data.DOM.Simple.Events (preventDefault, MouseEventType(MouseClickEvent), addMouseEventListener)
import Data.DOM.Simple.Types (DOMEvent())
import qualified Data.DOM.Simple.Window as W
import Data.Maybe (Maybe(..))
import DOM (DOM())

start :: forall eff. Eff (dom :: DOM | eff) Unit
start = do
  doc <- W.document W.globalWindow
  maybeButton <- querySelector ".search-button" doc
  case maybeButton of
    Nothing -> return unit
    Just button -> addMouseEventListener MouseClickEvent handleClickButton button

handleClickButton :: forall eff. DOMEvent -> Eff (dom :: DOM | eff) Unit
handleClickButton e = do
  preventDefault e
  doc <- W.document W.globalWindow
  maybeButton <- querySelector ".search-button" doc
  case maybeButton of
    Nothing -> return unit
    Just button -> do
      setAttribute "disabled" "" button
      classAdd "disabled" button
  maybeSpinner <- querySelector ".spinner" doc
  case maybeSpinner of
    Nothing -> return unit
    Just spinner -> setStyleAttr "display" "block" spinner

-- Dummy function because psc-bundle needs to call something
main :: String -> String
main _ = "noop"
