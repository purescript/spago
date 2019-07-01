module Spago.Search.App.SearchField where

import Prelude

import CSS hiding (render, map)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KeyboardEvent

type State = { input :: String }

data Action
  = InputAction String
  | EnterPressed
  | EscapePressed
  | FocusChanged Boolean

data SearchFieldMessage
  = InputUpdated String
  | InputCleared
  | Focused
  | LostFocus

component :: forall q i. H.Component HH.HTML q i SearchFieldMessage Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { input: "" }

handleAction :: forall m. Action -> H.HalogenM State Action () SearchFieldMessage m Unit
handleAction = case _ of
  InputAction input -> do
    H.modify_ $ const { input }
  EnterPressed -> do
    state <- H.get
    H.raise $ InputUpdated state.input
  EscapePressed -> do
    H.modify_ (_ { input = "" })
    H.raise $ InputCleared
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
    , HE.onKeyUp (\event ->
                   case KeyboardEvent.code event of
                     "Enter"  -> Just EnterPressed
                     "Escape" -> Just EscapePressed
                     _        -> Nothing)
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
