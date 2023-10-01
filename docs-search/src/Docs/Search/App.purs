-- | This is the main module of the client-side Halogen app.
module Docs.Search.App where

import Docs.Search.App.SearchField as SearchField
import Docs.Search.App.SearchResults as SearchResults
import Docs.Search.App.Sidebar as Sidebar
import Docs.Search.Config as Config
import Docs.Search.Extra (whenJust)
import Docs.Search.ModuleIndex as ModuleIndex
import Docs.Search.PackageIndex as PackageIndex
import Docs.Search.Meta as Meta

import Prelude

import Control.Alt (alt)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription (subscribe)
import Halogen.VDom.Driver (runUI)
import MarkdownIt as MD
import Web.DOM.ChildNode as ChildNode
import Web.DOM.Document as Document
import Web.DOM.Document (Document)
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.ParentNode as ParentNode
import Web.DOM.Text as Text
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML as HTML
import Web.HTML.Event.EventTypes (focus)
import Web.HTML.Event.HashChangeEvent.EventTypes (hashchange)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (fromElement)
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  window <- HTML.window

  insertStyle
  insertVersionInfo
  mbContainers <- getContainers

  -- Initialize a `markdown-it` instance (we need it to render the docs as markdown)
  markdownIt <- MD.newMarkdownIt MD.Default mempty

  whenJust mbContainers
    \{ searchField
     , searchResults
     , pageContents
     , sidebarContainer
     , realSidebar
     , isIndexHTML
     } -> do

      -- Hide real sidebar completely - we are going to recreate it as Halogen component.
      ChildNode.remove $ Element.toChildNode realSidebar

      HA.runHalogenAff do
        packageIndex <- PackageIndex.loadPackageIndex
        moduleIndex <- ModuleIndex.unpackModuleIndex <$> ModuleIndex.loadModuleIndex
        meta <- Meta.load
        let scores = PackageIndex.mkScoresFromPackageIndex packageIndex

        let
          initialSearchEngineState =
            { packageIndex
            , moduleIndex
            , index: wrap Map.empty
            , typeIndex: wrap Map.empty
            , scores
            }
          resultsComponent =
            SearchResults.mkComponent
              initialSearchEngineState
              pageContents
              markdownIt
              meta

        sfio <- runUI SearchField.component unit searchField
        srio <- runUI resultsComponent unit searchResults

        void $ H.liftEffect $ subscribe sfio.messages $ \sfm -> do
          launchAff_ $ void do
            srio.query (SearchResults.MessageFromSearchField sfm unit)

        -- We need to read the URI hash only when both components are initialized and
        -- the search field is subscribed to the main component.
        void $ sfio.query $ SearchField.ReadURIHash unit

        -- Subscribe to URI hash updates
        H.liftEffect do

          listener <-
            eventListener \_event ->
              launchAff_ $ void do
                sfio.query $ SearchField.ReadURIHash unit

          addEventListener hashchange listener true (Window.toEventTarget window)

        sbio <- do
          component <- Sidebar.mkComponent moduleIndex isIndexHTML meta
          runUI component unit sidebarContainer

        -- Subscribe to window focus events
        H.liftEffect do

          listener <-
            eventListener \_event ->
              launchAff_ $ void do
                sbio.query $ Sidebar.UpdateModuleGrouping unit

          addEventListener focus listener true (Window.toEventTarget window)

insertStyle :: Effect Unit
insertStyle = do
  doc <- getDocument

  let
    styleContents =
      """
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
  .li-package > details > summary {
    font-weight: bold;
    cursor: pointer;
    color: #c4953a;
  }
  .li-package > details > summary:hover {
    color: #7b5904;
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

insertVersionInfo :: Effect Unit
insertVersionInfo = do
  doc <- getDocument
  let docPN = Document.toParentNode doc
  mbVersionInfo <-
    ParentNode.querySelector (wrap ".footer > p") docPN
  whenJust (mbVersionInfo <#> Element.toNode)
    \versionInfo -> do
      prefix <- Document.createTextNode " - patched by " doc <#> Text.toNode
      linkElement <- Document.createElement "a" doc
      let linkNode = Element.toNode linkElement
      Element.setAttribute "href" "https://github.com/purescript/purescript-docs-search" linkElement
      Element.setAttribute "target" "_blank" linkElement
      linkText <- Document.createTextNode ("docs-search") doc <#> Text.toNode
      suffix <- Document.createTextNode (" " <> Config.version) doc <#> Text.toNode
      void $ Node.appendChild prefix versionInfo
      void $ Node.appendChild linkNode versionInfo
      void $ Node.appendChild linkText linkNode
      void $ Node.appendChild suffix versionInfo

-- | Query the DOM for specific elements that should always be present and determine if we are on
-- | `index.html` or not.
getContainers
  :: Effect
       ( Maybe
           { searchField :: HTML.HTMLElement
           , searchResults :: HTML.HTMLElement
           , pageContents :: Element.Element
           , sidebarContainer :: HTML.HTMLElement
           , realSidebar :: Element.Element
           , isIndexHTML :: Sidebar.IsIndexHTML
           }
       )
getContainers = do
  doc <- getDocument
  let docPN = Document.toParentNode doc
  mbBanner <-
    ParentNode.querySelector (wrap ".top-banner > .container") docPN
  mbEverything <-
    ParentNode.querySelector (wrap ".everything-except-footer") docPN
  mbContainer <-
    ParentNode.querySelector (wrap ".everything-except-footer > .container") docPN
  mbMainContainer <-
    ParentNode.querySelector (wrap ".everything-except-footer > main") docPN
  mbSidebarStatus <-
    alt
      <$>
        ( map (Tuple Sidebar.NotIndexHTML) <$>
            ParentNode.querySelector (wrap ".col--aside") docPN
        )
      -- If there's no sidebar, that means we are currently on `index.html`.
      <*>
        ( map (Tuple Sidebar.IsIndexHTML) <$>
            ParentNode.querySelector (wrap ".col--main") docPN
        )
  case unit of
    _
      | Just banner <- mbBanner
      , Just everything <- mbEverything
      , Just pageContents <- mbContainer
      , Just mainContainer <- mbMainContainer
      , Just (isIndexHTML /\ realSidebar) <- mbSidebarStatus -> do
          search <- Document.createElement "div" doc
          void $ Node.appendChild (Element.toNode search) (Element.toNode banner)
          pure do
            searchField <- fromElement search
            searchResults <- fromElement everything
            sidebarContainer <- fromElement mainContainer
            pure
              { searchField
              , searchResults
              , pageContents
              , realSidebar
              , sidebarContainer
              , isIndexHTML
              }
      | otherwise -> pure Nothing

getDocument :: Effect Document
getDocument = HTML.window >>= map HTMLDocument.toDocument <<< Window.document
