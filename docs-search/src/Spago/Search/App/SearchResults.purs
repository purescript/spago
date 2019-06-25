module Spago.Search.App.SearchResults where

import Prelude

import Spago.Search.App.SearchField (Message(..))
import Spago.Search.Declarations (loadDeclarations)
import Spago.Search.Extra (whenJust)
import Spago.Search.Index (SearchIndex, SearchResult, mkSearchIndex)

import CSS (textWhitespace, whitespacePreWrap)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..), isNothing, isJust)
import Data.Newtype (unwrap, wrap)
import Data.Search.Trie as Trie
import Data.String (length) as String
import Data.String.CodeUnits (toCharArray, stripSuffix) as String
import Data.String.Common (toLower) as String
import Data.String.Pattern (Pattern(..)) as String
import Effect.Aff (Aff)
import Effect.Console (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window

type State = { mbIndex :: Maybe SearchIndex
             , results :: Array SearchResult
             , input :: String
             , contents :: Element
             }

data Query a
  = SearchFieldMessage Message a

data Action
  = SearchResultClicked String

mkComponent :: forall o i. Element -> H.Component HH.HTML Query i o Aff
mkComponent contents =
  H.mkComponent
    { initialState: const { mbIndex: Nothing, results: [], input: "", contents }
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery
                                     , handleAction = handleAction }
    }

handleQuery :: forall o a. Query a -> H.HalogenM State Action () o Aff (Maybe a)
handleQuery (SearchFieldMessage Focused next) = do
  state <- H.get
  when (isNothing state.mbIndex) do
    eiDeclarations <- H.liftAff $ loadDeclarations "../spago-search-index.js"
    case eiDeclarations of
      Left err -> do
        H.liftEffect do
          error $ "spago-search: couldn't load search index: " <> err
      Right declarations -> do
        H.modify_ (_ { mbIndex = Just $ mkSearchIndex declarations })
  pure Nothing
handleQuery (SearchFieldMessage LostFocus next) = do
  state <- H.get
  when (Array.null state.results) do
    showPageContents
  pure Nothing
handleQuery (SearchFieldMessage (InputUpdated input) next) = do
  H.modify_ (_ { input = input })
  if String.length input < 2
  then do
    showPageContents
    H.modify_ (_ { results = [] })
  else do
    hidePageContents
    state <- H.get
    whenJust (unwrap <$> state.mbIndex) \index -> do
      let path = List.fromFoldable $
                 String.toCharArray $
                 String.toLower input
      H.modify_ (_ { results = Array.concat $
                               List.toUnfoldable $
                               map List.toUnfoldable $
                               Trie.queryValues path $
                               index })
  pure Nothing

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  SearchResultClicked moduleName -> do
    -- Decide if we are going to load a new page or to jump to a hash on the
    -- current page. In the latter case, hide search results and show the main
    -- page contents.
    onThisPage <- H.liftEffect do
      window <- HTML.window
      location <- Window.location window
      pathname <- Location.pathname location
      pure $ isJust $
        String.stripSuffix (String.Pattern $ moduleName <> ".html") pathname

    when onThisPage do
      showPageContents
      H.modify_ (_ { input = "" })

showPageContents :: forall o. H.HalogenM State Action () o Aff Unit
showPageContents = do
  state <- H.get
  H.liftEffect do
    Element.removeAttribute "style" state.contents

hidePageContents :: forall o. H.HalogenM State Action () o Aff Unit
hidePageContents = do
  state <- H.get
  H.liftEffect do
    Element.setAttribute "style" "display: none" state.contents

render :: forall m. State -> H.ComponentHTML Action () m
render { mbIndex: Nothing } =
  HH.div_ []
render { input: "" } =
  HH.div_ []
render state =
  HH.div [ HP.classes [ wrap "container", wrap "clearfix" ] ] $
  pure $

  HH.div [ HP.classes [ wrap "col", wrap "col--main" ] ] $

  [ HH.h1_ [ HH.text "Search results" ] ] <>

  if Array.null state.results
  then
    [ HH.div [ HP.classes [ wrap "result", wrap "result--empty" ] ]
      [ HH.text "Your search for "
      , HH.strong_ [ HH.text state.input ]
      , HH.text " did not yield any results."
      ]
    ]
  else
    [ renderSummary $
      "Found " <>
      show (Array.length state.results) <>
      " definitions" ] <>

    [ HH.div [ HP.id_ "spage-search-results-container" ] $
      Array.concat $
      state.results <#> renderResult
    ]

renderSummary :: forall a b. String -> HH.HTML b a
renderSummary text =
  HH.div [ HP.id_ "spago-search-summary" ]
  [ HH.text text
  ]

renderResult :: forall a.  SearchResult -> Array (HH.HTML a Action)
renderResult = unwrap >>> \result ->
  [ HH.div [ HP.class_ (wrap "result") ]
    [ HH.h3 [ HP.class_ (wrap "result__title") ]
      [ HH.a [ HP.class_ (wrap "result__link")
             , HE.onClick $ const $ Just $ SearchResultClicked result.moduleName
             , HP.href $
               result.moduleName <> ".html#" <>
               result.hashAnchor <> ":" <> result.name
             ]
        [ HH.text result.name ]
      ]
    ]

  , HH.div [ HP.class_ (wrap "result__body") ] $
    [ HH.pre [ HP.class_ (wrap "result__signature") ]
      [ HH.code_ []
      ]
    ] <>

    case result.comments of
      Just comments -> [ HH.pre [ HS.style do
                                     textWhitespace whitespacePreWrap ]
                         [ HH.text comments ]
                       ]
      Nothing -> [ ]

  , HH.div [ HP.class_ (wrap "result__actions") ]
    [ HH.span [ HP.class_ (wrap "result__actions__item") ]
      [ HH.span [ HP.classes [ wrap "badge"
                             , wrap "badge--package"
                             ]
                , HP.title "Package"
                ]
        [ HH.text "P"
        ]
      , HH.text result.packageName
      ]

    , HH.span [ HP.class_ (wrap "result__actions__item") ]
      [ HH.span [ HP.classes [ wrap "badge"
                             , wrap "badge--module"
                             ]
                , HP.title "Module"
                ]
        [ HH.text "M"
        ]
      , HH.text result.moduleName
      ]
    ]
  ]
