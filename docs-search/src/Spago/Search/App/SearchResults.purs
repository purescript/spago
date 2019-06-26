module Spago.Search.App.SearchResults where

import Prelude
import Spago.Search.Index
import Spago.Search.TypeDecoder

import CSS (textWhitespace, whitespacePreWrap)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), (:))
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
import Spago.Search.App.SearchField (Message(..))
import Spago.Search.DocsJson (loadDeclarations)
import Spago.Search.Extra (whenJust)
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window

type State = { mbIndex :: Maybe SearchIndex
             , results :: Array SearchResult
             , input :: String
             , contents :: Element
             , resultsCount :: Int
             }

data Query a
  = SearchFieldMessage Message a

data Action
  = SearchResultClicked String
  | ShowMore

mkComponent :: forall o i. Element -> H.Component HH.HTML Query i o Aff
mkComponent contents =
  H.mkComponent
    { initialState: const { mbIndex: Nothing
                          , results: []
                          , input: ""
                          , contents
                          , resultsCount: 25
                          }
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
                               index
                   , resultsCount = 25 })
  pure Nothing

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  ShowMore -> do
    H.modify_ (\state -> state { resultsCount = state.resultsCount + 25 })
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
    let selectedResults = Array.take state.resultsCount state.results in
    [ HH.div [ HP.classes [ wrap "result" ] ]
      [ HH.text "Found "
      , HH.strong_ [ HH.text $ show $ Array.length state.results ]
      , HH.text " definitions"
      ]

    , HH.div [ HP.id_ "spage-search-results-container" ] $
      Array.concat $ selectedResults <#> renderResult

    , HH.div [ HP.class_ (wrap "load_more"), HP.id_ "load-more" ]
      [ if Array.length selectedResults < Array.length state.results
        then HH.a [ HP.id_ "load-more-link"
                  , HE.onClick $ const $ Just ShowMore ]
             [ HH.text "Show more results" ]
        else HH.p_
             [ HH.text "No further results." ]
      ]
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
    renderResultType result <>

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

renderResultType
  :: forall a rest
  .  { info :: ResultInfo
     , name :: String
     | rest
     }
 -> Array (HH.HTML a Action)
renderResultType = \result ->
  case result.info of
    ValueResult {type:ty} ->
      [ HH.pre [ HP.class_ (wrap "result__signature") ]
        [ HH.code_ [ HH.text (result.name <> " :: ")
                   , renderType ty ]
        ]
      ]
    _ -> []

renderType :: forall a. Type -> HH.HTML a Action
renderType = case _ of
  TypeVar str -> HH.text str
  TypeLevelString str -> HH.text str
  TypeWildcard -> HH.text "_"
  TypeConstructor qname -> renderQualifiedName qname
  TypeOp qname -> renderQualifiedName qname

  TypeApp (TypeApp (TypeConstructor
                    (QualifiedName { moduleName: ["Prim"]
                                   , name: "Function" })) t1) t2 ->
    HH.span_ [ renderType t1
             , HH.text " -> "
             , renderType t2
             ]

  TypeApp (TypeConstructor (QualifiedName { moduleName: ["Prim"]
                                          , name: "Record" }))
          record -> renderRow record

  TypeApp t1 t2 ->
    HH.span_ [ renderType t1
             , HH.text " "
             , renderType t2
             ]

  ty@(ForAll _ _ _) ->
    let foralls = joinForalls ty in
    HH.span_ $
    [ HH.text "forall" ] <>
    ( Array.fromFoldable foralls.vars <#>
      \ { var, mbKind } ->
        case mbKind of
          Nothing -> HH.text (" " <> var)
          Just kind ->
            HH.span_ [ HH.text ("(" <> var <> " :: ")
                     , renderKind kind
                     , HH.text ")"
                     ]
    ) <>
    [ HH.text ". ", renderType foralls.ty ]

  ConstrainedType cnstr ty ->
    HH.span_
    [ renderConstraint cnstr
    , HH.text " => "
    , renderType ty
    ]

  REmpty -> HH.text "{}"
  ty@(RCons _ _ _) -> renderRow ty

  BinaryNoParensType t1 t2 t3 ->
    HH.span_
    [ renderType t1
    , renderType t2
    , renderType t3
    ]
  ParensInType ty ->
    HH.span_
    [ HH.text "("
    , renderType ty
    , HH.text ")"
    ]

joinForalls :: Type -> { vars :: List.List { var :: String, mbKind :: Maybe Kind }
                       , ty :: Type }
joinForalls = go Nil
  where
    go acc (ForAll var ty mbKind) =
      go ({ var, mbKind } : acc) ty
    go acc ty = { vars: acc, ty }

renderRow :: forall a. Type -> HH.HTML a Action
renderRow ty =
  let rows = joinRows ty in
  HH.span_ $
  [ HH.text "{ " ] <>
  ( Array.intercalate [ HH.text ", " ] $ Array.fromFoldable rows <#>
    \entry ->
    [ HH.span_ [ HH.text $ entry.row <> " :: "
               , renderType entry.ty ] ]
  ) <>
  [ HH.text " }" ]

joinRows :: Type -> List { row :: String
                         , ty :: Type
                         }
joinRows = go Nil
  where
    go acc (RCons row ty rest) =
      go ({ row, ty } : acc) rest
    go acc _ = List.reverse acc


htmlSingleton :: forall t406 t407. HH.HTML t407 t406 -> HH.HTML t407 t406
htmlSingleton x = HH.span_ [ x ]

renderConstraint :: forall a. Constraint -> HH.HTML a Action
renderConstraint (Constraint { constraintClass, constraintArgs }) =
  HH.span_ $
  [ renderQualifiedName constraintClass, HH.text " " ] <>
  Array.intercalate [ HH.text " " ] (constraintArgs <#> \ty -> [ renderType ty ])


renderQualifiedName :: forall a. QualifiedName -> HH.HTML a Action
renderQualifiedName (QualifiedName { moduleName, name })
  = HH.text name

renderKind :: forall a. Kind -> HH.HTML a Action
renderKind = case _ of
  Row k1 -> HH.span_ [ HH.text "#", renderKind k1 ]
  FunKind k1 k2 -> HH.span_ [ renderKind k1, renderKind k2 ]
  NamedKind qname -> renderQualifiedName qname
