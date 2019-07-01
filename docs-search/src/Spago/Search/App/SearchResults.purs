module Spago.Search.App.SearchResults where

import Prelude

import Spago.Search.App.SearchField (SearchFieldMessage(..))
import Spago.Search.Config (config)
import Spago.Search.Declarations (DeclLevel(..), declLevelToHashAnchor)
import Spago.Search.DocsJson (DataDeclType(..))
import Spago.Search.Extra ((>#>))
import Spago.Search.Index (Index)
import Spago.Search.Index as Index
import Spago.Search.SearchResult (ResultInfo(..), SearchResult, typeOf)
import Spago.Search.TypeDecoder (Constraint(..), FunDep(..), FunDeps(..), Kind(..), QualifiedName(..), Type(..), TypeArgument(..), joinForAlls, joinRows)
import Spago.Search.TypeIndex (TypeIndex)
import Spago.Search.TypeIndex as TypeIndex
import Spago.Search.TypeQuery (TypeQuery(..), parseTypeQuery, penalty)

import CSS (textWhitespace, whitespacePreWrap)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (hush)
import Data.List as List
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.String (length) as String
import Data.String.CodeUnits (stripSuffix) as String
import Data.String.Common (toLower, trim) as String
import Data.String.Pattern (Pattern(..)) as String
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window

data Mode = Off | Loading | Active | InputTooShort

derive instance eqMode :: Eq Mode

-- | Is it a search by type or by name?
data ResultsType = TypeResults TypeQuery | DeclResults

type State = { index :: Index
             , typeIndex :: TypeIndex
             , results :: Array SearchResult
             , resultsType :: ResultsType
             , input :: String
             , contents :: Element
             , resultsCount :: Int
             , mode :: Mode
             }

data Query a
  = MessageFromSearchField SearchFieldMessage a

data Action
  = SearchResultClicked String
  | MoreResultsRequested

mkComponent
  :: forall o i
  .  Element
  -> H.Component HH.HTML Query i o Aff
mkComponent contents =
  H.mkComponent
    { initialState: const { index: mempty
                          , typeIndex: mempty
                          , results: []
                          , resultsType: DeclResults
                          , input: ""
                          , contents
                          , resultsCount: config.resultsCount
                          , mode: Off
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

  if String.length input < 2
  then do
    if input == ""
    then do
      H.modify_ (_ { mode = Off })
      showPageContents
    else do
      H.modify_ (_ { mode = InputTooShort })
      hidePageContents
  else do
    H.modify_ (_ { mode = Loading, resultsCount = config.resultsCount })

    void $ H.fork do
      let resultsType =
            maybe DeclResults TypeResults (hush (parseTypeQuery state.input)
                                           >>= isValuableTypeQuery)

      case resultsType of

        DeclResults -> do
          { index, results } <- H.liftAff $ Index.query state.index (String.toLower state.input)
          H.modify_ (_ { results = results
                       , mode = Active
                       , index = index })

        TypeResults query -> do
          { index, results } <- H.liftAff $ TypeIndex.query state.typeIndex query
          H.modify_ (_ { results = sortByDistance query results
                       , mode = Active
                       , typeIndex = index })

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

showPageContents
  :: forall o
  .  H.HalogenM State Action () o Aff Unit
showPageContents = do
  state <- H.get
  H.liftEffect do
    Element.removeAttribute "style" state.contents

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
render { mode: InputTooShort } =
  renderContainer $
  [ HH.h1_ [ HH.text "Error" ] ] <>
  [ HH.div [ HP.classes [ wrap "result", wrap "result--empty" ] ]
    [ HH.text "Search query is too short." ]
  ]
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

  , HH.div [ HP.classes [ wrap "result" ] ] $
    [ HH.text "Found "
    , HH.strong_ [ HH.text $ show $ Array.length state.results ]
    , HH.text $
        case state.resultsType of
          DeclResults   -> " definitions."
          TypeResults _ -> " definitions with similar types."
    ]

  , HH.div_ $
    Array.concat $ shownResults <#> renderResult

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
  HH.div [ HP.id_ "spago-search-summary" ]
  [ HH.text text ]

renderResult
  :: forall a
  .  SearchResult
  -> Array (HH.HTML a Action)
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

    result.comments >#>
      \comments -> [ HH.pre [ HS.style do
                                 textWhitespace whitespacePreWrap ]
                     [ HH.text comments ]
                   ]

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
      wrapSignature [ HH.a [ makeHref ValueLevel false result.moduleName result.name
                           , HE.onClick $ const $ Just $ SearchResultClicked result.moduleName ]
                      [ HH.text result.name ]
                    , HH.text " :: "
                    , renderType ty ]

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
     , typeClassArguments :: Array String
     }
  -> { name :: String | rest }
  -> Array (HH.HTML a Action)
renderTypeClassMemberSignature { type: ty, typeClass, typeClassArguments } result =
  [ HH.text result.name
  , HH.text " :: "
  , renderType ty ]

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
  TypeLevelString str -> HH.text $ "(Text \"" <> str <> "\")" -- TODO: add escaping
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
    \ { var, mbKind } ->
    case mbKind of
      Nothing -> HH.text (" " <> var)
      Just kind ->
        HH.span_ [ HH.text $ " (" <> var <> " "
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
    [ HH.text $ if asRow then "()" else "{}" ]
  else
    [ HH.text opening ] <>

    ( Array.intercalate [ HH.text ", " ] $ Array.fromFoldable $ rows <#>
      \entry ->
      [ HH.span_ [ HH.text $ entry.row <> " :: "
                 , renderType entry.ty ] ]
    ) <>

    case ty of
      Just ty' -> [ HH.text " | ", renderType ty', HH.text closing ]
      Nothing  -> [ HH.text closing ]

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
  .  Kind ->
  HH.HTML a Action
renderKind = case _ of
  Row k1          -> HH.span_ [ HH.text "# ", renderKind k1 ]
  FunKind k1 k2   -> HH.span_ [ renderKind k1, syntax " -> ", renderKind k2 ]
  NamedKind qname -> renderQualifiedName false KindLevel qname

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

isValuableTypeQuery :: TypeQuery -> Maybe TypeQuery
isValuableTypeQuery (QVar _) = Nothing
isValuableTypeQuery (QConst _) = Nothing
isValuableTypeQuery query = Just query

sortByDistance :: TypeQuery -> Array SearchResult -> Array SearchResult
sortByDistance typeQuery results =
  _.result <$> Array.sortBy comparePenalties resultsWithPenalties
  where
    comparePenalties r1 r2 = compare r1.penalty r2.penalty
    resultsWithPenalties = results <#>
               \result -> { penalty: typeOf (unwrap result).info >>= penalty typeQuery
                          , result }
