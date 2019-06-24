module Spago.Search.App.SearchResults where

import Prelude

import Spago.Search.App.SearchField (Message(..))
import Spago.Search.Declarations
import Spago.Search.Index
import Spago.Search.Extra

import CSS hiding (render,map)
import Data.Array as Array
import Data.List as List
import Data.Maybe
import Data.Newtype
import Data.Search.Trie as Trie
import Data.String (null) as String
import Data.String.CodeUnits (toCharArray) as String
import Data.String.Common (toLower) as String
import Effect.Aff
import Effect.Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { shown :: Boolean
             , mbIndex :: Maybe SearchIndex
             , results :: Array SearchResult
             , input :: String
             }

data Query a
  = SearchFieldMessage Message a

component :: forall i o. H.Component HH.HTML Query i o Aff
component =
  H.mkComponent
    { initialState:
      const { mbIndex: Nothing, results: [], shown: false, input: "" }
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }

handleQuery :: forall o a. Query a -> H.HalogenM State Unit () o Aff (Maybe a)
handleQuery (SearchFieldMessage Focused next) = do
  H.modify_ (_ { shown = true })
  state <- H.get
  when (isNothing state.mbIndex) do
    declarations <- H.liftAff $ loadDeclarations "../spago-search-index.js"
    H.modify_ (_ { mbIndex = Just $ mkSearchIndex declarations })
  pure Nothing
handleQuery (SearchFieldMessage LostFocus next) = do
  state <- H.get
  when (Array.null state.results) do
    H.modify_ (_ { shown = false })
  pure Nothing
handleQuery (SearchFieldMessage (InputUpdated input) next) = do
  H.modify_ (_ { input = input })

  if String.null input
  then do
    H.modify_ (_ { results = [] })
  else do
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

render :: forall m. State -> H.ComponentHTML Unit () m
render { mbIndex: Nothing, shown: true } =
  renderSummary "Loading index..."
render { mbIndex: Nothing, shown: false } =
  HH.div_ []
render state
  | not state.shown || String.null state.input = HH.div_ []
  | otherwise =
    HH.div
    [ HS.style do
         float floatLeft
    ]
    if Array.null state.results
    then
      [ renderSummary "No search results" ]
    else
      [ renderSummary $
        "Found " <>
        show (Array.length state.results) <>
        " definitions" ] <>

      [ HH.div_ $ state.results <#> \result ->
         HH.div_ [ HH.text (unwrap result).name ]
      ]

renderSummary :: forall a b. String -> HH.HTML b a
renderSummary text =
  HH.div [ HP.id_ "spago-search-summary" ]
  [ HH.text text
  ]
