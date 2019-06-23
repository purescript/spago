module Spago.Search.App.SearchField where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HS
import CSS as CSS
import Effect.Aff
import Data.Maybe

type State = { input :: String }

data Action
  = InputAction String

data Message = InputUpdated String

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

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
  [ HS.style do
       CSS.float CSS.floatLeft
       CSS.lineHeight (CSS.px 90.0)
       CSS.marginBottom (CSS.px 0.0)
       CSS.width (CSS.pct 50.0)
  ]
  [ HH.input
    [ HP.value state.input
    , HP.type_ HP.InputText
    , HE.onValueInput (Just <<< InputAction)
    , HS.style do
      let color = CSS.rgb 0x1d 0x22 0x2d
          br = CSS.px 3.0
      CSS.border CSS.solid (CSS.px 1.0) color
      CSS.borderRadius br br br br
      CSS.color color
      CSS.fontWeight (CSS.weight 300.0)
      CSS.lineHeight (CSS.em 2.0)
      CSS.paddingLeft (CSS.em 0.21)
      CSS.paddingRight (CSS.em 0.21)
      CSS.paddingTop (CSS.em 0.512)
      CSS.paddingBottom (CSS.em 0.512)
      CSS.width (CSS.pct 100.0)
    ]
  ]
