module Docs.Search.App.SearchField where

import Prelude

import CSS (border, borderRadius, color, em, float, floatLeft, fontWeight, lineHeight, marginBottom, marginLeft, paddingBottom, paddingLeft, paddingRight, paddingTop, pct, px, rgb, solid, weight, width)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Web.DOM.Document as Document
import Web.DOM.ParentNode as ParentNode
import Web.HTML (window) as Web
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (blur, focus, fromElement) as Web
import Web.HTML.Window (document) as Web
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State = { input :: String, focused :: Boolean }

data Action
  = InputAction String
  | EnterPressed
  | FocusChanged Boolean
  | InitKeyboardListener
  | HandleKey H.SubscriptionId KeyboardEvent

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
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just InitKeyboardListener }
    }

initialState :: forall i. i -> State
initialState _ = { input: "", focused: false }

handleAction :: Action -> H.HalogenM State Action () SearchFieldMessage Aff Unit
handleAction = case _ of

  InitKeyboardListener -> do
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \sid ->
      ES.eventListenerEventSource
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)

  HandleKey sid ev -> do
    when (KE.code ev == "KeyS") do
      H.liftEffect $ withSearchField Web.focus
    when (KE.code ev == "Escape") do
      state <- H.get
      if state.focused
      then H.liftEffect $ withSearchField Web.blur
      else do
        H.modify_ (_ { input = "" })
        H.raise $ InputCleared

  InputAction input -> do
    H.modify_ $ (_ { input = input })

  EnterPressed -> do
    state <- H.get
    H.liftEffect $ withSearchField Web.blur
    H.raise $ InputUpdated state.input

  FocusChanged status -> do
    H.modify_ (_ { focused = status })
    H.raise
      if status
      then Focused
      else LostFocus

withSearchField :: (HTML.HTMLElement -> Effect Unit) -> Effect Unit
withSearchField cont = do
  doc <- Document.toParentNode <$>
         HTMLDocument.toDocument <$>
         (Window.document =<< HTML.window)

  let selector = wrap "#docs-search-query-field"

  mbEl <- ParentNode.querySelector selector doc
  maybe mempty cont (mbEl >>= Web.fromElement)

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
    , HP.id_ "docs-search-query-field"
    , HP.type_ HP.InputText
    , HE.onKeyUp (\event ->
                   case KeyboardEvent.code event of
                     "Enter"  -> Just EnterPressed
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
