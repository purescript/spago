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
import Spago.Search.Extra
import Web.DOM as DOM
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Text as Text
import Web.DOM.Node as Node
import Web.DOM.ParentNode as ParentNode
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Control.Coroutine as Coroutine
import Halogen as H

main :: Effect Unit
main = do
  win <- HTML.window
  doc <- HTMLDocument.toDocument <$> Window.document win

  insertStyle doc
  mbContainer <- findContainer doc

  whenJust mbContainer \container -> do
    HA.runHalogenAff do
      sfio <- runUI SearchField.component unit container
      srio <- runUI SearchResults.component unit container
      sfio.subscribe $
        Coroutine.consumer \message ->
        srio.query $ H.tell $ SearchResults.SearchFieldMessage message

insertStyle :: Document.Document -> Effect Unit
insertStyle doc = do
  let styleContents = """
  .top-banner__actions {
    width: 10%;
  }
  """
  mbHead <-
    ParentNode.querySelector (wrap "head") (Document.toParentNode doc)

  whenJust mbHead \head -> do

    contents <- Document.createTextNode styleContents doc
    style <- Document.createElement "style" doc
    void $ Node.appendChild (Text.toNode contents) (Element.toNode style)
    void $ Node.appendChild (Element.toNode style) (Element.toNode head)

findContainer :: Document.Document -> Effect (Maybe HTML.HTMLElement)
findContainer doc = do
  mbBanner <-
    ParentNode.querySelector (wrap ".top-banner > .container") (Document.toParentNode doc)
  case mbBanner of
    Nothing -> pure Nothing
    Just banner -> do
      container <- Document.createElement "div" doc
      void $ Node.appendChild (Element.toNode container) (Element.toNode banner)
      pure $ HTMLElement.fromElement container
