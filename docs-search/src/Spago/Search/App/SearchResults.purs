module Spago.Search.App.SearchResults where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HS
import CSS as CSS
import Effect.Aff
import Effect.Console
import Spago.Search.Index
import Spago.Search.Declarations
import Data.Maybe
import Data.Search.Trie as Trie
import Data.Newtype
import Data.String.CodeUnits as String
import Data.List as List
import Data.Array as Array
import Data.String as String

type State = { index :: SearchIndex, results :: Array IndexEntry }

data Message = InputUpdated String

data Query a = Input String a

mkComponent :: forall i o. SearchIndex -> H.Component HH.HTML Query i o Aff
mkComponent index =
  H.mkComponent
    { initialState:
      const { index, results: [] }
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }

handleQuery :: forall o a. Query a -> H.HalogenM State Unit () o Aff (Maybe a)
handleQuery (Input input next) = do
  state <- H.get

  if String.null input
  then do
    H.put $ state { results = [] }
  else do

    let path = List.fromFoldable $
               String.toCharArray input

    H.put $ state { results = List.toUnfoldable $
                              Trie.queryValues path $
                              unwrap state.index }

  pure Nothing

render :: forall m. State -> H.ComponentHTML Unit () m
render state =
  HH.div_
  if Array.null state.results
  then []
  else
    [ HH.text $ "Search results: " <> show (Array.length state.results)
    , HH.div_ $ state.results <#> \result ->
       HH.div_ [ HH.text (unwrap result).title ]
    ]
