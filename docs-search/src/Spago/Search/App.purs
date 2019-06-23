module Spago.Search.App where

import Effect.Console
import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Spago.Search.App.SearchField as SearchField
import Spago.Search.App.SearchResults as SearchResults
import Spago.Search.Declarations (Declarations)
import Spago.Search.Index (mkSearchIndex)
import Web.DOM as DOM
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.ParentNode as ParentNode
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Control.Coroutine
import Halogen as H

main :: Effect Unit
main = launchAff_ do
  declarations <- loadDeclarations "../spago-search-index.js"
  let index = mkSearchIndex declarations
  mbContainer <- liftEffect findContainer

  case mbContainer of
    Nothing -> do
      pure unit

    Just container ->
      liftEffect do
        HA.runHalogenAff do
          sfio <- runUI SearchField.component unit container
          srio <- runUI (SearchResults.mkComponent index) unit container
          sfio.subscribe $
            consumer \(SearchField.InputUpdated text) -> do
              srio.query $ H.tell $ SearchResults.Input text

findContainer :: Effect (Maybe HTML.HTMLElement)
findContainer = do
  win <- HTML.window
  doc <- HTMLDocument.toDocument <$> Window.document win
  mbBanner <- ParentNode.querySelector (wrap ".top-banner > .container") (Document.toParentNode doc)
  case mbBanner of
    Nothing -> pure Nothing
    Just banner -> do
      container <- Document.createElement "div" doc
      void $ Node.appendChild (Element.toNode container) (Element.toNode banner)
      pure $ HTMLElement.fromElement container

loadDeclarations :: String -> Aff (Array Declarations)
loadDeclarations = toAffE <<< loadDeclarations_

foreign import loadDeclarations_ :: String -> Effect (Promise (Array Declarations))
