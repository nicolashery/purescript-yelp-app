module App.UI where

import Prelude

import Data.Foldable (traverse_)
import qualified Text.Smolder.HTML as H
import qualified Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (Markup(), text, (!))
import Text.Smolder.Renderer.String (render)

import App.Model (SearchQuery(..), ApiError(..), Business(..))

renderPageToString :: Markup -> String
renderPageToString body =
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
          body
          H.script ! A.src "bundle.js" $ text ""
  in render markup

renderSearchPageToString :: SearchQuery -> Markup -> String
renderSearchPageToString query content =
  let body = H.div $ do
        renderPoweredByYelp
        renderSearchForm query
        renderSpinner
        content
  in renderPageToString body

renderPoweredByYelp :: Markup
renderPoweredByYelp =
  H.div ! A.className "column row" $
    H.img ! A.className "powered-by-yelp" ! A.src "powered-by-yelp.png"

renderSearchForm :: SearchQuery -> Markup
renderSearchForm (SearchQuery { term = term, location = location }) =
  H.form ! A.className "search-form" $ do
    H.div ! A.className "row" $ do
      H.div ! A.className "medium-4 columns" $ do
        H.label $ do
          text "Find"
          H.input ! A.type' "text" ! A.name "term" ! A.value term
      H.div ! A.className "medium-4 columns" $ do
        H.label $ do
          text "Near"
          H.input ! A.type' "text" ! A.name "location" ! A.value location
      H.div ! A.className "medium-4 columns" $ do
        H.button ! A.type' "submit" ! A.className "search-button expanded button" $
          text "Search"

renderWelcomeMessage :: Markup
renderWelcomeMessage =
  H.div ! A.className "column row" $
    text "Search for bars, restaurants, etc. in your city."

renderError :: forall a. (Show a) => a -> Markup
renderError error =
  H.div ! A.className "alert callout" $ do
    H.p $ text "Oops! Something went wrong."
    H.p $ text (show error)

renderApiError :: ApiError -> Markup
renderApiError (ApiError { name = name, message = message }) =
  renderError ("(" ++ name ++ ") " ++ message)

renderResults :: SearchQuery ->  Array Business -> Markup
renderResults query [] = renderNoResults query
renderResults _ businesses = renderBusinesses businesses

renderNoResults :: SearchQuery -> Markup
renderNoResults (SearchQuery { term = term, location = location }) =
  H.div ! A.className "column row" $ do
    text "We couldn't find and results for "
    H.strong $ text term
    text " near "
    H.strong $ text location
    text "."

renderBusinesses :: Array Business -> Markup
renderBusinesses businesses =
  H.ul ! A.className "business-list" $ traverse_ renderBusiness businesses

renderBusiness :: Business -> Markup
renderBusiness (Business { name = name }) =
  H.li ! A.className "column row" $
    H.div ! A.className "business" $ text name

renderSpinner :: Markup
renderSpinner =
  H.div ! A.className "spinner sk-fading-circle" $ do
    H.div ! A.className "sk-circle1 sk-circle" $ text ""
    H.div ! A.className "sk-circle2 sk-circle" $ text ""
    H.div ! A.className "sk-circle3 sk-circle" $ text ""
    H.div ! A.className "sk-circle4 sk-circle" $ text ""
    H.div ! A.className "sk-circle5 sk-circle" $ text ""
    H.div ! A.className "sk-circle6 sk-circle" $ text ""
    H.div ! A.className "sk-circle7 sk-circle" $ text ""
    H.div ! A.className "sk-circle8 sk-circle" $ text ""
    H.div ! A.className "sk-circle9 sk-circle" $ text ""
    H.div ! A.className "sk-circle10 sk-circle" $ text ""
    H.div ! A.className "sk-circle11 sk-circle" $ text ""
    H.div ! A.className "sk-circle12 sk-circle" $ text ""
