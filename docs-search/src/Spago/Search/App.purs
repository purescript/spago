module Spago.Search.App where

import Prelude

import Spago.Search.Declarations (Declarations)
import Spago.Search.Index (mkSearchIndex)

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Web.DOM as DOM
import Web.DOM.Document as Document
import Web.DOM.ParentNode as ParentNode
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.DOM.Node as Node

main :: Effect Unit
main = launchAff_ do
  declarations <- loadDeclarations "../spago-search-index.js"
  elem <- liftEffect findContainer
  let index = mkSearchIndex declarations
  pure unit

findContainer :: Effect (Maybe DOM.Element)
findContainer = do
  win <- HTML.window
  doc <- HTMLDocument.toDocument <$> Window.document win
  mbBanner <- ParentNode.querySelector (wrap ".top-banner") (Document.toParentNode doc)
  case mbBanner of
    Nothing -> pure Nothing
    Just banner -> do
      container <- Document.createElement "div" doc
      void $ Node.appendChild (Element.toNode container) (Element.toNode banner)
      pure $ Just container

loadDeclarations :: String -> Aff (Array Declarations)
loadDeclarations = toAffE <<< loadDeclarations_

foreign import loadDeclarations_ :: String -> Effect (Promise (Array Declarations))
