module Spago.Search.App.SearchResults where

import Prelude

import Spago.Search.Declarations (loadDeclarations)
import Spago.Search.Extra (whenJust)
import Spago.Search.Index (SearchIndex, SearchResult, mkSearchIndex)

import Data.Array as Array
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Search.Trie as Trie
import Data.String (length) as String
import Data.String.CodeUnits (toCharArray) as String
import Data.String.Common (toLower) as String
import Effect.Aff (Aff)
import Effect.Console (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Spago.Search.App.SearchField (Message(..))
import Web.DOM.Element (Element)
import Web.DOM.Element as Element

type State = { mbIndex :: Maybe SearchIndex
             , results :: Array SearchResult
             , input :: String
             , contents :: Element
             }

data Query a
  = SearchFieldMessage Message a

mkComponent :: forall i o. Element -> H.Component HH.HTML Query i o Aff
mkComponent contents =
  H.mkComponent
    { initialState:
      const { mbIndex: Nothing, results: [], input: "", contents }
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }

handleQuery :: forall o a. Query a -> H.HalogenM State Unit () o Aff (Maybe a)
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

showPageContents :: forall o. H.HalogenM State Unit () o Aff Unit
showPageContents = do
  state <- H.get
  H.liftEffect do
    Element.removeAttribute "style" state.contents

hidePageContents :: forall o. H.HalogenM State Unit () o Aff Unit
hidePageContents = do
  state <- H.get
  H.liftEffect do
    Element.setAttribute "style" "display: none" state.contents

render :: forall m. State -> H.ComponentHTML Unit () m
render { mbIndex: Nothing } =
  HH.div_ []
render { input: "" } =
  HH.div_ []
render state =
  HH.div [ HP.classes [ wrap "container"
                      , wrap "clearfix"
                      ]
         ]
  if Array.null state.results
  then
    [ renderSummary "No search results" ]
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

renderResult :: forall a b.  SearchResult -> Array (HH.HTML a b)
renderResult = unwrap >>> \result ->
  [ HH.div [ HP.class_ (wrap "result") ]
    [ HH.h3 [ HP.class_ (wrap "result__title") ]
      [ HH.a [ HP.class_ (wrap "result__link") ]
        [ HH.text result.name ]
      ]
    ]

  , HH.div [ HP.class_ (wrap "result__body") ] $
    [ HH.pre [ HP.class_ (wrap "result__signature") ]
      [ HH.code_ []
      ]
    ] <>

    case result.comments of
      Just comments -> [ HH.pre_
                         [ HH.text comments
                         ]
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
