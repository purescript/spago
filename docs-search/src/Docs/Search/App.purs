-- | This is the main module of the client-side Halogen app.
module Docs.Search.App where

import Docs.Search.App.SearchField as SearchField
import Docs.Search.App.SearchResults as SearchResults
import Docs.Search.App.Sidebar as Sidebar
import Docs.Search.Extra (whenJust)
import Docs.Search.ModuleIndex as ModuleIndex
import Docs.Search.PackageIndex as PackageIndex

import Prelude
import Control.Coroutine as Coroutine
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MarkdownIt as MD
import Web.DOM.ChildNode as ChildNode
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.ParentNode as ParentNode
import Web.DOM.Text as Text
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML as HTML
import Web.HTML.Event.HashChangeEvent.EventTypes (hashchange)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (fromElement)
import Web.HTML.Window as Window
import Web.HTML.Event.EventTypes (focus)


main :: Effect Unit
main = do
  win <- HTML.window
  doc <- HTMLDocument.toDocument <$> Window.document win

  insertStyle doc
  mbContainers <- getContainers doc

  -- Initialize a `markdown-it` instance (we need it to render the docs as markdown)
  markdownIt <- MD.newMarkdownIt MD.Default mempty

  whenJust mbContainers \ { searchField
                          , searchResults
                          , pageContents
                          , sidebarContainer
                          , realSidebar } -> do

    -- Hide real sidebar completely - we are going to recreate it as Halogen component.
    ChildNode.remove $ Element.toChildNode realSidebar

    HA.runHalogenAff do
      packageIndex <- PackageIndex.loadPackageIndex
      moduleIndex <- ModuleIndex.loadModuleIndex

      let initialSearchEngineState = { packageIndex: packageIndex
                                     , moduleIndex: moduleIndex
                                     , index: mempty
                                     , typeIndex: mempty
                                     }

          resultsComponent =
            SearchResults.mkComponent initialSearchEngineState pageContents markdownIt

      sfio <- runUI SearchField.component unit searchField
      sbio <- do
              component <- Sidebar.mkComponent moduleIndex
              runUI component unit sidebarContainer
      srio <- runUI resultsComponent unit searchResults

      sfio.subscribe $
        Coroutine.consumer (srio.query <<< H.tell <<< SearchResults.MessageFromSearchField)

      -- We need to read the URI hash only when both components are initialized and
      -- the search field is subscribed to the main component.
      void $ sfio.query $ H.tell SearchField.ReadURIHash

      -- Subscribe to URI hash updates
      H.liftEffect do

        listener <-
          eventListener \event ->
            launchAff_ do
              sfio.query $ H.tell SearchField.ReadURIHash

        addEventListener hashchange listener true (Window.toEventTarget win)

      -- Subscribe to window focus events
      H.liftEffect do

        listener <-
          eventListener \event ->
            launchAff_ do
              sbio.query $ H.tell Sidebar.UpdateModuleGrouping

        addEventListener focus listener true (Window.toEventTarget win)


insertStyle :: Document.Document -> Effect Unit
insertStyle doc = do
  let styleContents = """
  .top-banner__actions {
    width: 10%;
  }
  .load_more {
    margin-top:2em;
  }
  .load_more p {
    font-style:italic
  }
  .load_more a {
    background:#eee;
    padding:0.4em
  }
  #load-more-link {
    cursor: pointer;
  }
  .result {
    font-size: 1.25em;
  }
  .result__body .keyword, .result__body .syntax {
    color: #0B71B4;
  }
  .badge {
    /* Add a margin between badge icons and package/module names. */
    margin-right: 0.25em;
  }
  .li-package {
    font-weight: bold;
    cursor: pointer;
    color: #c4953a;
  }
  /* Make spaces narrower in the sidebar */
  .li-package > details > ul {
    margin-top: auto;
    margin-bottom: auto;
  }
  #group-modules__label, #group-modules__input {
    display: inline-block;
    vertical-align: middle;
    color: #666666;
    font-size: 0.8rem;
    font-weight: 300;
    letter-spacing: 1px;
    margin-bottom: -0.8em;
  }
  summary:focus {
    outline: none;
  }
  """

  mbHead <-
    ParentNode.querySelector (wrap "head") (Document.toParentNode doc)

  whenJust mbHead \head -> do
    contents <- Document.createTextNode styleContents doc
    style <- Document.createElement "style" doc
    void $ Node.appendChild (Text.toNode contents) (Element.toNode style)
    void $ Node.appendChild (Element.toNode style) (Element.toNode head)


-- | Query the DOM for specific elements that should always be present.
getContainers
  :: Document.Document
  -> Effect (Maybe { searchField :: HTML.HTMLElement
                   , searchResults :: HTML.HTMLElement
                   , pageContents :: Element.Element
                   , sidebarContainer :: HTML.HTMLElement
                   , realSidebar :: Element.Element
                   })
getContainers doc = do
  let docPN = Document.toParentNode doc
  mbBanner <-
    ParentNode.querySelector (wrap ".top-banner > .container") docPN
  mbEverything <-
    ParentNode.querySelector (wrap ".everything-except-footer") docPN
  mbContainer <-
    ParentNode.querySelector (wrap ".everything-except-footer > .container") docPN
  mbMainContainer <-
    ParentNode.querySelector (wrap ".everything-except-footer > main") docPN
  mbRealSidebar <-
    ParentNode.querySelector (wrap ".col--aside") docPN
  case unit of
    _ | Just banner        <- mbBanner
      , Just everything    <- mbEverything
      , Just pageContents  <- mbContainer
      , Just mainContainer <- mbMainContainer
      , Just realSidebar   <- mbRealSidebar -> do
      search <- Document.createElement "div" doc
      void $ Node.appendChild (Element.toNode search) (Element.toNode banner)
      pure do
        searchField <- fromElement search
        searchResults <- fromElement everything
        sidebarContainer <- fromElement mainContainer
        pure { searchField, searchResults, pageContents, realSidebar, sidebarContainer }
      | otherwise -> pure Nothing
