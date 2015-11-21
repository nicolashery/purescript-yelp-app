module App.UI where

import Prelude

import Data.Foldable (traverse_)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (Markup(), text, (!))
import Text.Smolder.Renderer.String (render)

import App.Model (Business(..))

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
          H.img ! A.className "powered-by-yelp" ! A.src "powered-by-yelp.png"
          content
  in render markup

renderBusinesses :: Array Business -> Markup
renderBusinesses businesses =
  H.ul ! A.className "business-list" $ traverse_ renderBusiness businesses

renderBusiness :: Business -> Markup
renderBusiness (Business { name = name }) =
  H.li ! A.className "business" $ text name

renderBusinessesToString :: Array Business -> String
renderBusinessesToString businesses = render (renderBusinesses businesses)
