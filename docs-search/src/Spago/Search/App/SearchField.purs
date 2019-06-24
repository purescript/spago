module Spago.Search.App.SearchField where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HS
import CSS hiding (render, map)
import Effect.Aff (Aff)
import Data.Maybe (Maybe(..))

type State = { input :: String }

data Action
  = InputAction String
  | FocusChanged Boolean

data Message
  = InputUpdated String
  | Focused
  | LostFocus

component :: forall q i. H.Component HH.HTML q i Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { input: "" }

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  InputAction input -> do
    H.modify_ $ const { input }
    H.raise $ InputUpdated input
  FocusChanged status -> do
    H.raise
      if status
      then Focused
      else LostFocus

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div

  [ HS.style do
       float floatLeft
       lineHeight (px 90.0)
       marginBottom (px 0.0)
       marginLeft (em 2.0)
       width (pct 30.0)
  ]

  [ HH.input
    [ HP.value state.input
    , HP.placeholder "Search for definitions"
    , HP.type_ HP.InputText
    , HE.onValueInput (Just <<< InputAction)
    , HE.onFocusIn  $ const $ Just $ FocusChanged true
    , HE.onFocusOut $ const $ Just $ FocusChanged false
    , HS.style do

      let pursuitColor = rgb 0x1d 0x22 0x2d
          rds = px 3.0

      border solid (px 1.0) pursuitColor
      borderRadius rds rds rds rds
      color pursuitColor
      fontWeight    $ weight 300.0
      lineHeight    $ em 2.0
      paddingLeft   $ em 0.8
      paddingRight  $ em 0.21
      paddingTop    $ em 0.512
      paddingBottom $ em 0.512
      width         $ pct 100.0
    ]
  ]
