-- | This module contains a Halogen component for search results.
module Docs.Search.App.SearchResults where

import Docs.Search.App.SearchField (SearchFieldMessage(..))
import Docs.Search.BrowserEngine (PartialIndex, browserSearchEngine)
import Docs.Search.Config (config)
import Docs.Search.Declarations (DeclLevel(..), declLevelToHashAnchor)
import Docs.Search.DocsJson (DataDeclType(..))
import Docs.Search.Extra (homePageFromRepository, (>#>))
import Docs.Search.PackageIndex (PackageResult)
import Docs.Search.Engine (Result(..))
import Docs.Search.Engine as Engine
import Docs.Search.SearchResult (ResultInfo(..), SearchResult(..))
import Docs.Search.TypeDecoder (Constraint(..), FunDep(..), FunDeps(..), Kind(..), QualifiedName(..), Type(..), TypeArgument(..), joinForAlls, joinRows)
import Docs.Search.TypeIndex (TypeIndex)

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Newtype (wrap)
import Data.String.CodeUnits (stripSuffix) as String
import Data.String.Common (null, trim) as String
import Data.String.Pattern (Pattern(..)) as String
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MarkdownIt as MD
import MarkdownIt.Renderer.Halogen as MDH
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window


data Mode = Off | Loading | Active

derive instance eqMode :: Eq Mode


type EngineState =
  Engine.EngineState PartialIndex TypeIndex


type State = { engineState :: EngineState
             , results :: Array Result
             , input :: String
             , contents :: Element
             , resultsCount :: Int
             , mode :: Mode
             , markdownIt :: MD.MarkdownIt
             }


data Query a
  = MessageFromSearchField SearchFieldMessage a


data Action
  = SearchResultClicked String
  | MoreResultsRequested


mkComponent
  :: forall o i
  .  EngineState
  -> Element
  -> MD.MarkdownIt
  -> H.Component HH.HTML Query i o Aff
mkComponent initialEngineState contents markdownIt =
  H.mkComponent
    { initialState: const { engineState: initialEngineState
                          , results: []
                          , input: ""
                          , contents
                          , resultsCount: config.resultsCount
                          , mode: Off
                          , markdownIt
                          }
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery
                                     , handleAction = handleAction }
    }


handleQuery
  :: forall o a
  .  Query a
  -> H.HalogenM State Action () o Aff (Maybe a)
handleQuery (MessageFromSearchField Focused next) = do
  pure Nothing
handleQuery (MessageFromSearchField LostFocus next) = do
  pure Nothing
handleQuery (MessageFromSearchField InputCleared next) = do
  H.modify_ (_ { results = [], input = "", mode = Off })
  showPageContents
  pure Nothing
handleQuery (MessageFromSearchField (InputUpdated input_) next) = do
  let input = String.trim input_

  state <- H.modify (_ { input = input })

  if String.null input
  then do
      H.modify_ (_ { mode = Off })
      showPageContents
  else do
    H.modify_ (_ { mode = Loading, resultsCount = config.resultsCount })

    void $ H.fork do
      { index, results } <- H.liftAff $
        Engine.query browserSearchEngine state.engineState state.input

      H.modify_ (_ { results = results
                   , mode = Active
                   , engineState = index
                   }
                )

    hidePageContents

  pure Nothing


handleAction
 :: forall o
 .  Action
 -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  MoreResultsRequested -> do
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
      H.modify_ (_ { input = "", mode = Off })


-- | Inverse of `hidePageContents`
showPageContents
  :: forall o
  .  H.HalogenM State Action () o Aff Unit
showPageContents = do
  state <- H.get
  H.liftEffect do
    Element.removeAttribute "style" state.contents


-- | When search UI is active, we want to hide the main page contents.
hidePageContents
  :: forall o
  .  H.HalogenM State Action () o Aff Unit
hidePageContents = do
  state <- H.get
  H.liftEffect do
    Element.setAttribute "style" "display: none" state.contents


render
  :: forall m
  .  State
  -> H.ComponentHTML Action () m
render { mode: Off } = HH.div_ []
render { mode: Loading } =
  renderContainer $
  [ HH.h1_ [ HH.text "Loading..." ] ]
render state@{ mode: Active, results: [] } =
  renderContainer $

  [ HH.h1_ [ HH.text "Search results" ]
  , HH.div [ HP.classes [ wrap "result", wrap "result--empty" ] ]
    [ HH.text "Your search for "
    , HH.strong_ [ HH.text state.input ]
    , HH.text " did not yield any results."
    ]
  ]

render state@{ mode: Active } =
  renderContainer $
  [ HH.h1_ [ HH.text "Search results" ]

  , HH.div_ $
    Array.concat $ shownResults <#> renderResult state.markdownIt

  , HH.div [ HP.class_ (wrap "load_more"), HP.id_ "load-more" ]
    [ if Array.length shownResults < Array.length state.results
      then HH.a [ HP.id_ "load-more-link"
                , HE.onClick $ const $ Just MoreResultsRequested ]
           [ HH.text "Show more results" ]
      else HH.p_
           [ HH.text "No further results." ]
    ]
  ]
  where
    shownResults = Array.take state.resultsCount state.results


renderContainer :: forall a b. Array (HH.HTML b a) -> HH.HTML b a
renderContainer =
  HH.div [ HP.classes [ wrap "container", wrap "clearfix" ] ] <<<
  pure <<<
  HH.div [ HP.classes [ wrap "col", wrap "col--main" ] ]


renderSummary
  :: forall a b
  .  String
  -> HH.HTML b a
renderSummary text =
  HH.div_ [ HH.text text ]


renderResult
  :: forall a
  .  MD.MarkdownIt
  -> Result
  -> Array (HH.HTML a Action)
renderResult markdownIt (DeclResult sr) = renderSearchResult markdownIt sr
renderResult markdownIt (TypeResult sr) = renderSearchResult markdownIt sr
renderResult markdownIt (PackResult sr) = renderPackageResult sr


renderPackageResult
  :: forall a
  .  PackageResult
  -> Array (HH.HTML a Action)
renderPackageResult { name, description, repository } =
  [ HH.div [ HP.class_ (wrap "result") ]
    [ HH.h3 [ HP.class_ (wrap "result__title") ]
      [ HH.span [ HP.classes [ wrap "result__badge"
                             , wrap "badge"
                             , wrap "badge--package" ]
                , HP.title "Package"
                ]
        [ HH.text "P" ]

      , HH.a [ HP.class_ (wrap "result__link")
             , HP.href $ fromMaybe "" repository # homePageFromRepository
             ]
        [ HH.text name ]
      ]
    ]
  ] <>

  description >#> \descriptionText ->
  [ HH.div [ HP.class_ (wrap "result__body") ]
    [ HH.text descriptionText ]
  ]


renderSearchResult
  :: forall a
  .  MD.MarkdownIt
  -> SearchResult
  -> Array (HH.HTML a Action)
renderSearchResult markdownIt (SearchResult result) =
  -- class names here and below are from Pursuit.
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

    result.comments >#> pure <<< MDH.render_ markdownIt

  , HH.div [ HP.class_ (wrap "result__actions") ]

    [ HH.span [ HP.class_ (wrap "result__actions__item") ]
      [ HH.span [ HP.classes [ wrap "badge"
                             , wrap "badge--package"
                             ]
                , HP.title "Package"
                ]
        [ HH.text "P" ]
      , HH.text result.packageName
      ]

    , HH.span [ HP.class_ (wrap "result__actions__item") ]
      [ HH.span [ HP.classes [ wrap "badge"
                             , wrap "badge--module"
                             ]
                , HP.title "Module"
                ]
        [ HH.text "M" ]
      , HH.text result.moduleName
      ]
    ]
  ]


renderResultType
  :: forall a rest
  .  { info :: ResultInfo
     , name :: String
     , moduleName :: String
     | rest
     }
 -> Array (HH.HTML a Action)
renderResultType result =

  case result.info of
    ValueResult { type: ty } ->
      wrapSignature $ renderValueSignature result ty

    TypeClassResult info ->
      wrapSignature $ renderTypeClassSignature info result

    TypeClassMemberResult info ->
      wrapSignature $ renderTypeClassMemberSignature info result

    DataResult info ->
      wrapSignature $ renderDataSignature info result

    TypeSynonymResult info ->
      wrapSignature $ renderTypeSynonymSignature info result
    _ -> []

  where
    wrapSignature signature =
      [ HH.pre [ HP.class_ (wrap "result__signature") ] [ HH.code_ signature ] ]


renderValueSignature
  :: forall a rest
  .  { moduleName :: String
     , name :: String
     | rest
     }
  -> Type
  -> Array (HH.HTML a Action)
renderValueSignature result ty =
  [ HH.a [ makeHref ValueLevel false result.moduleName result.name
         , HE.onClick $ const $ Just $ SearchResultClicked result.moduleName ]
    [ HH.text result.name ]
  , HH.text " :: "
  , renderType ty ]


renderTypeClassSignature
  :: forall a rest
  .  { fundeps :: FunDeps
     , arguments :: Array TypeArgument
     , superclasses :: Array Constraint
     }
  -> { name :: String, moduleName :: String | rest }
  -> Array (HH.HTML a Action)
renderTypeClassSignature { fundeps, arguments, superclasses } { name, moduleName } =
  [ keyword "class"
  , if Array.null superclasses
    then
      HH.text ""
    else
      HH.span_ $
      [ syntax " ("
      , HH.span_ $ Array.intercalate [ HH.text ", " ] (
        superclasses <#> renderConstraint >>> Array.singleton
      )
      , syntax ")"
      , space
      , syntax "<="
      ]
  , space
  , HH.a [ makeHref TypeLevel false moduleName name
         , HE.onClick $ const $ Just $
           SearchResultClicked moduleName
         ]
    [ HH.text name ]
  , space
  ] <> (
    Array.intercalate [ space ] $
      arguments <#> renderTypeArgument
  ) <> (
    renderFunDeps fundeps
  )


renderFunDeps :: forall a. FunDeps -> Array (HH.HTML a Action)
renderFunDeps (FunDeps []) = []
renderFunDeps (FunDeps deps) =
  append [ syntax " | " ] $
  Array.intercalate [ syntax ", " ] $
  deps <#> renderFunDep
  where
    renderFunDep (FunDep { lhs, rhs }) =
      Array.intercalate [ space ] (pure <<< HH.text <$> lhs) <>
      [ syntax " -> " ] <>
      Array.intercalate [ space ] (pure <<< HH.text <$> rhs)


-- | Insert type class name and arguments
renderTypeClassMemberSignature
  :: forall a rest
  .  { type :: Type
     , typeClass :: QualifiedName
     , typeClassArguments :: Array TypeArgument
     }
  -> { name :: String | rest }
  -> Array (HH.HTML a Action)
renderTypeClassMemberSignature { type: ty, typeClass, typeClassArguments } result =
  [ HH.text result.name
  , HH.text " :: "
  , renderType ty
  ]


renderDataSignature
  :: forall a rest
  .  { typeArguments :: Array TypeArgument
     , dataDeclType :: DataDeclType }
  -> { name :: String | rest }
  -> Array (HH.HTML a Action)
renderDataSignature { typeArguments, dataDeclType } { name } =
  [ keyword
    case dataDeclType of
      NewtypeDataDecl -> "newtype"
      DataDataDecl    -> "data"
  , space
  , HH.text name
  , space
  , HH.span_ $
    Array.intercalate [ space ] $
      typeArguments <#> renderTypeArgument
  ]


renderTypeSynonymSignature
  :: forall a rest
  .  { type :: Type
     , arguments :: Array TypeArgument
     }
  -> { name :: String | rest }
  -> Array (HH.HTML a Action)
renderTypeSynonymSignature { type: ty, arguments } { name } =
  [ keyword "type"
  , space
  , HH.text name
  , space
  , HH.span_ $
    Array.intercalate [ space ] $
      arguments <#> renderTypeArgument
  , space
  , syntax "="
  , space
  , renderType ty
  ]


renderTypeArgument :: forall a. TypeArgument -> Array (HH.HTML a Action)
renderTypeArgument (TypeArgument { name, mbKind }) =
  case mbKind of
    Nothing ->
      [ HH.text name ]
    Just kind ->
      [ HH.text "("
      , HH.text name
      , HH.text " :: "
      , renderKind kind
      , HH.text ")"
      ]


renderType
  :: forall a
  .  Type
  -> HH.HTML a Action
renderType = case _ of
  TypeVar str -> HH.text str
  TypeLevelString str -> HH.text $ "\"" <> str <> "\"" -- TODO: add escaping
  TypeWildcard -> HH.text "_"
  TypeConstructor qname -> renderQualifiedName false TypeLevel qname
  TypeOp qname -> renderQualifiedName true TypeLevel qname

  TypeApp (TypeApp (TypeConstructor
                    (QualifiedName { moduleName: [ "Prim" ]
                                   , name: "Function" })) t1) t2 ->
    HH.span_ [ renderType t1
             , syntax " -> "
             , renderType t2
             ]

  TypeApp (TypeConstructor (QualifiedName { moduleName: [ "Prim" ]
                                          , name: "Record" }))
          row ->
    renderRow false row

  TypeApp t1 t2 ->
    HH.span_ [ renderType t1
             , space
             , renderType t2
             ]

  ty@(ForAll _ _ _) ->
    renderForAll ty

  ConstrainedType cnstr ty ->
    HH.span_
    [ renderConstraint cnstr
    , HH.text " => "
    , renderType ty
    ]

  ty@REmpty -> renderRow true ty
  ty@(RCons _ _ _) -> renderRow true ty

  BinaryNoParensType op t1 t2 ->
    HH.span_
    [ renderType t1
    , space
    , renderType op
    , space
    , renderType t2
    ]

  ParensInType ty ->
    HH.span_
    [ HH.text "("
    , renderType ty
    , HH.text ")"
    ]


renderForAll
  :: forall a
  .  Type
  -> HH.HTML a Action
renderForAll ty =
  HH.span_ $

  [ keyword "forall" ] <>

  ( Array.fromFoldable foralls.binders <#>
    \ { name, mbKind } ->
    case mbKind of
      Nothing -> HH.text (" " <> name)
      Just kind ->
        HH.span_ [ HH.text $ " (" <> name <> " "
                 , syntax "::"
                 , space
                 , renderKind kind
                 , HH.text ")" ]
  ) <>

  [ syntax ". ", renderType foralls.ty ]

  where
    foralls = joinForAlls ty


renderRow
  :: forall a
  .  Boolean
  -> Type
  -> HH.HTML a Action
renderRow asRow =
  joinRows >>> \ { rows, ty } ->

  HH.span_ $

  if List.null rows
  then
    [ if asRow
      then HH.text "()"
      else
        fromMaybe (HH.text "{}") $
        ty <#> \ty' ->
        HH.span_
        [ renderQualifiedName false TypeLevel primRecord
        , HH.text " "
        , renderType ty'
        ]
    ]
  else
    [ HH.text opening ] <>

    ( Array.intercalate [ HH.text ", " ] $ Array.fromFoldable $ rows <#>
      \entry ->
      [ HH.span_ [ HH.text $ entry.row <> " :: "
                 , renderType entry.ty ] ]
    ) <>

    (ty >#> \ty' -> [ HH.text " | ", renderType ty' ]) <>

    [ HH.text closing ]

    where
      opening = if asRow then "( " else "{ "
      closing = if asRow then " )" else " }"


renderConstraint
  :: forall a
  .  Constraint
  -> HH.HTML a Action
renderConstraint (Constraint { constraintClass, constraintArgs }) =
  HH.span_ $
  [ renderQualifiedName false TypeLevel constraintClass, space ] <>
  Array.intercalate [ space ] (constraintArgs <#> \ty -> [ renderType ty ])


renderQualifiedName
  :: forall a
  .  Boolean
  -> DeclLevel
  -> QualifiedName
  -> HH.HTML a Action
renderQualifiedName isInfix level (QualifiedName { moduleName, name })
  = if isBuiltIn then
      HH.text name
    else
      HH.a [ HE.onClick $ const $ Just $
             SearchResultClicked moduleNameString
           , makeHref level isInfix moduleNameString name
           ]
      [ HH.text name ]
      where
        moduleNameString = Array.intercalate "." moduleName
        isBuiltIn = moduleName !! 0 == Just "Prim"


renderKind
  :: forall a
  .  Kind
  -> HH.HTML a Action
renderKind = case _ of
  Row k1          -> HH.span_ [ HH.text "# ", renderKind k1 ]
  FunKind k1 k2   -> HH.span_ [ renderKind k1, syntax " -> ", renderKind k2 ]
  NamedKind qname -> renderQualifiedName false KindLevel qname


-- | Construct a `href` property value w.r.t. `DeclLevel`.
makeHref
  :: forall t rest
  .  DeclLevel
  -> Boolean
  -> String
  -> String
  -> HH.IProp ( href :: String | rest ) t
makeHref level isInfix moduleName name =
  HP.href $
  moduleName <> ".html#" <>
  declLevelToHashAnchor level <> ":" <>
  if isInfix then "type (" <> name <> ")" else name


primRecord :: QualifiedName
primRecord = QualifiedName { moduleName: [ "Prim" ], name: "Record" }


keyword
  :: forall a
  .  String
  -> HH.HTML a Action
keyword str = HH.span [ HP.class_ (wrap "keyword") ] [ HH.text str ]


syntax
  :: forall a
  .  String
  -> HH.HTML a Action
syntax str = HH.span [ HP.class_ (wrap "syntax") ] [ HH.text str ]


space :: forall a b. HH.HTML a b
space = HH.text " "
