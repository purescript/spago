-- | This module contains a Halogen component for search field, that emits
-- | `SearchFieldMessage`s for various events.
module Docs.Search.App.SearchField where

import Prelude

import CSS (border, borderRadius, color, em, float, floatLeft, fontWeight, lineHeight, marginBottom, marginLeft, paddingBottom, paddingLeft, paddingRight, paddingTop, pct, px, rgb, solid, weight, width)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Docs.Search.URIHash as URIHash
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as ES
import Web.DOM.Document as Document
import Web.DOM.ParentNode as ParentNode
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
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
  | NoOp

data Query a = ReadURIHash a

data SearchFieldMessage
  = InputUpdated String
  | InputCleared
  | Focused
  | LostFocus

component :: forall i. H.Component Query i SearchFieldMessage Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just InitKeyboardListener
        }
    }

handleQuery
  :: forall a
   . Query a
  -> H.HalogenM State Action () SearchFieldMessage Aff (Maybe a)
handleQuery (ReadURIHash _next) = do
  oldInput <- H.get <#> _.input
  newInput <- H.liftEffect URIHash.getInput
  when (oldInput /= newInput) do
    H.modify_ (_ { input = newInput })
    H.raise (InputUpdated newInput)
  pure Nothing

initialState :: forall i. i -> State
initialState _ = { input: "", focused: false }

handleAction :: Action -> H.HalogenM State Action () SearchFieldMessage Aff Unit
handleAction = case _ of
  NoOp -> pure unit
  InitKeyboardListener -> do

    document <- H.liftEffect $ Window.document =<< HTML.window
    H.subscribe' \sid ->
      ES.eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)

  HandleKey _sid ev -> do

    when (KE.code ev == "KeyS") do
      state <- H.get
      when (not state.focused) do
        H.liftEffect do
          withSearchField HTMLInputElement.select
          withSearchField (HTMLInputElement.toHTMLElement >>> HTMLElement.focus)

    when (KE.code ev == "Escape") do
      state <- H.get
      if state.focused then do
        H.liftEffect do
          withSearchField (HTMLInputElement.toHTMLElement >>> HTMLElement.blur)
      else clearInput

  InputAction input -> do
    H.modify_ $ (_ { input = input })

  EnterPressed -> do
    state <- H.get
    H.liftEffect do
      withSearchField (HTMLInputElement.toHTMLElement >>> HTMLElement.blur)
    H.liftEffect (URIHash.setInput state.input)
    H.raise $ InputUpdated state.input

  FocusChanged isFocused -> do
    H.modify_ (_ { focused = isFocused })
    H.raise
      if isFocused then Focused
      else LostFocus
    when isFocused scrollToTop

scrollToTop :: H.HalogenM State Action () SearchFieldMessage Aff Unit
scrollToTop = do
  H.liftEffect do
    HTML.window >>= Window.scroll 0 0

clearInput :: H.HalogenM State Action () SearchFieldMessage Aff Unit
clearInput = do
  H.modify_ (_ { input = "" })
  H.liftEffect URIHash.removeHash
  H.raise InputCleared

withSearchField :: (HTML.HTMLInputElement -> Effect Unit) -> Effect Unit
withSearchField cont = do
  doc <- Document.toParentNode
    <$> HTMLDocument.toDocument
    <$>
      (Window.document =<< HTML.window)

  let selector = wrap "#docs-search-query-field"

  mbEl <- ParentNode.querySelector selector doc
  maybe mempty cont (mbEl >>= HTMLInputElement.fromElement)

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
        , HP.placeholder "Search for definitions... (S to focus)"
        , HP.id "docs-search-query-field"
        , HP.type_ HP.InputText
        , HE.onKeyUp
            ( \event ->
                case KeyboardEvent.code event of
                  "Enter" -> EnterPressed
                  _ -> NoOp
            )
        , HE.onValueInput InputAction
        , HE.onFocusIn $ const $ FocusChanged true
        , HE.onFocusOut $ const $ FocusChanged false
        , HS.style do

            let
              pursuitColor = rgb 0x1d 0x22 0x2d
              rds = px 3.0

            border solid (px 1.0) pursuitColor
            borderRadius rds rds rds rds
            color pursuitColor
            fontWeight $ weight 300.0
            lineHeight $ em 2.0
            paddingLeft $ em 0.8
            paddingRight $ em 0.21
            paddingTop $ em 0.512
            paddingBottom $ em 0.512
            width $ pct 100.0
        ]
    ]
