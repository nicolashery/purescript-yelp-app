module App.Client.Router where

import Prelude

import Control.Monad.Eff (Eff())

foreign import navigate :: forall eff. String -> Eff eff Unit

foreign import addUrlChangeListener :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit

foreign import start :: forall eff. Eff eff Unit
