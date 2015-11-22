module App.UI where

import Prelude

import Data.Foldable (traverse_)
import qualified Text.Smolder.HTML as H
import qualified Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (Markup(), text, (!))
import Text.Smolder.Renderer.String (render)

import App.Model (ApiError(..), Business(..))

renderPageToString :: Markup -> String
renderPageToString content =
  let markup =
      H.html ! A.lang "en" $ do
        H.head $ do
          H.meta ! A.charset "utf-8"
          H.meta ! A.httpEquiv "x-ua-compatible" ! A.content "ie=edge,chrome=1"
          H.meta ! A.name "viewport" ! A.content "width=device-width,initial-scale=1.0"
          H.title $ text "PureScript Yelp App"
          H.link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.0.1/css/foundation.min.css"
          H.link ! A.rel "stylesheet" ! A.href "style.css"
        H.body $ do
          renderPoweredByYelp
          content
  in render markup

renderPoweredByYelp :: Markup
renderPoweredByYelp = H.img ! A.className "powered-by-yelp" ! A.src "powered-by-yelp.png"

renderApiError :: ApiError -> Markup
renderApiError (ApiError { name = name, message = message }) =
  H.div ! A.className "alert callout" $ do
    H.p $ text "Oops! Something went wrong."
    H.p $ text ("(" ++ name ++ ") " ++ message)

renderBusinesses :: Array Business -> Markup
renderBusinesses businesses =
  H.ul ! A.className "business-list" $ traverse_ renderBusiness businesses

renderBusiness :: Business -> Markup
renderBusiness (Business { name = name }) =
  H.li ! A.className "business" $ text name
