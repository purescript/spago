module Spago.Search.App.SearchResults where

import Prelude

import Spago.Search.Index
import Spago.Search.TypeDecoder
import Spago.Search.TypeQuery
import Spago.Search.TypeShape

import CSS (textWhitespace, whitespacePreWrap)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Search.Trie as Trie
import Data.String (length) as String
import Data.String.CodeUnits (toCharArray, stripSuffix) as String
import Data.String.Common (toLower, trim) as String
import Data.String.Pattern (Pattern(..)) as String
import Effect.Aff (Aff)
import Effect.Console (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Spago.Search.App.SearchField (Message(..))
import Spago.Search.DocsJson (DataDeclType(..), loadDeclarations)
import Spago.Search.Extra (whenJust, (>#>))
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window

data Mode = Off | Loading | Active

derive instance eqMode :: Eq Mode

-- | Is it a search by type or by name?
data ResultsType = TypeResults TypeQuery | DeclResults

type State = { mbIndex :: Maybe SearchIndex
             , results :: Array SearchResult
             , resultsType :: ResultsType
             , input :: String
             , contents :: Element
             , resultsCount :: Int
             , mode :: Mode
             }

data Query a
  = SearchFieldMessage Message a

data Action
  = SearchResultClicked String
  | MoreResultsRequested

mkComponent
  :: forall o i
  .  Element
  -> H.Component HH.HTML Query i o Aff
mkComponent contents =
  H.mkComponent
    { initialState: const { mbIndex: Nothing
                          , results: []
                          , resultsType: DeclResults
                          , input: ""
                          , contents
                          , resultsCount: 25
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
handleQuery (SearchFieldMessage Focused next) = do
  pure Nothing
handleQuery (SearchFieldMessage LostFocus next) = do
  pure Nothing
handleQuery (SearchFieldMessage InputCleared next) = do
  H.modify_ (_ { results = [], input = "", mode = Off })
  updateSearchResults
  showPageContents
  pure Nothing
handleQuery (SearchFieldMessage (InputUpdated input) next) = do
  H.modify_ (_ { input = String.trim input })

  H.get >>= \state -> when (isNothing state.mbIndex && state.mode == Off) do
    H.modify_ (_ { mode = Loading })
    void $ H.fork do
      eiDeclarations <-
        H.liftAff $ loadDeclarations "../spago-search-index.js"
      case eiDeclarations of
        Left err -> do
          H.liftEffect do
            error $ "spago-search: couldn't load search index: " <> err
        Right declarations -> do
          H.modify_ (_ { mbIndex = Just $ mkSearchIndex declarations
                       , mode = Active })
          updateSearchResults

  updateSearchResults
  pure Nothing

updateSearchResults
  :: forall o
  .  H.HalogenM State Action () o Aff Unit
updateSearchResults = do
  { input } <- H.get

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

          eiTypeQuery = parseTypeQuery input

          resultsType =
            case eiTypeQuery of
              Left _ -> DeclResults
              Right query
                | isValuableTypeQuery query -> TypeResults query
                | otherwise -> DeclResults

          results =
            case resultsType of
              DeclResults ->
                Array.concat $
                List.toUnfoldable $
                map List.toUnfoldable $
                Trie.queryValues path $
                index.decls
              TypeResults query ->
                List.toUnfoldable $
                join $
                Trie.queryValues shape index.types
                where
                  shape = shapeOfTypeQuery query

      H.modify_ (_ { results = results
                   , resultsType = resultsType
                   , resultsCount = 25 })

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
      H.modify_ (_ { input = "" })

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
render { mode: Loading } =
  HH.div [ HP.classes [ wrap "container", wrap "clearfix" ] ] $
  pure $
  HH.div [ HP.classes [ wrap "col", wrap "col--main" ] ] $
  [ HH.h1_ [ HH.text "Loading..." ] ]

render state@{ mode: Active } =
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
    [ HH.div [ HP.classes [ wrap "result" ] ] $
      [ HH.text "Found "
      , HH.strong_ [ HH.text $ show $ Array.length state.results ]
      , HH.text $
          case state.resultsType of
            DeclResults   -> " definitions."
            TypeResults _ -> " definitions with similar types."
      ]

    , HH.div [ HP.id_ "spage-search-results-container" ] $
      Array.concat $ selectedResults <#> renderResult

    , HH.div [ HP.class_ (wrap "load_more"), HP.id_ "load-more" ]
      [ if Array.length selectedResults < Array.length state.results
        then HH.a [ HP.id_ "load-more-link"
                  , HE.onClick $ const $ Just MoreResultsRequested ]
             [ HH.text "Show more results" ]
        else HH.p_
             [ HH.text "No further results." ]
      ]
    ]
render { mode: Off } = HH.div_ []

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
      wrapSignature [ HH.a [ makeHref ValueLevel false result.moduleName result.name ]
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
  , HH.a [ makeHref TypeLevel false moduleName name ]
    [ HH.text name ]
  , space
  ] <> (
    Array.intercalate [ space ] $
      arguments <#> renderTypeArgument
  )

-- | Insert type class name and arguments
renderTypeClassMemberSignature
  :: forall a rest
  .  { type :: Type
     , typeClass :: String
     , typeClassArguments :: Array String
     }
  -> { name :: String | rest }
  -> Array (HH.HTML a Action)
renderTypeClassMemberSignature { type: ty, typeClass, typeClassArguments } result =
  [ HH.text result.name
  , HH.text " :: "
  , HH.text $ typeClass <> " "
    -- We don't want to insert `forall` here to avoid visual noise,
    -- and to make type class members more easily distinguishable from ordinary values.
    -- TODO: consider doing what pursuit does.
  , HH.text $ Array.intercalate " " typeClassArguments <> " "
  , HH.text "=> "
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
             SearchResultClicked $ moduleNameString
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

isValuableTypeQuery :: TypeQuery -> Boolean
isValuableTypeQuery (QVar _) = false
isValuableTypeQuery (QConst _) = false
isValuableTypeQuery _ = true
