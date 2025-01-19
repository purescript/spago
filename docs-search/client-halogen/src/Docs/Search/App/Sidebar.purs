module Docs.Search.App.Sidebar where

import Prelude

import Data.Array as Array
import Data.Lens ((.~))
import Data.Lens.Record (prop)
import Data.List (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Newtype (wrap, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Docs.Search.Config as Config
import Docs.Search.Meta (Meta)
import Docs.Search.ModuleIndex (ModuleIndex)
import Docs.Search.Types (ModuleName(..), PackageInfo(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Registry.PackageName as PackageName
import Type.Proxy (Proxy(..))
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.ParentNode as ParentNode
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

data Action = ToggleGrouping GroupingMode

data Query a = UpdateModuleGrouping a

data GroupingMode = GroupByPackage | DontGroup

derive instance groupingModeEq :: Eq GroupingMode

-- | Whether current page is `index.html` or not. `index.html` is special, it
-- | has no sidebar, hence the difference must be taken into account.
data IsIndexHTML = IsIndexHTML | NotIndexHTML

derive instance isIndexHTMLEq :: Eq IsIndexHTML

type State =
  { packageModules :: Map PackageInfo (Set ModuleName)
  , groupingMode :: GroupingMode
  , moduleNames :: Array ModuleName
  , isIndexHTML :: IsIndexHTML
  , currentPackage :: PackageInfo
  }

mkComponent
  :: forall i
   . ModuleIndex
  -> IsIndexHTML
  -> Meta
  -> Aff (H.Component Query i Action Aff)
mkComponent moduleIndex@{ packageModules } isIndexHTML {} = do
  groupingMode <- H.liftEffect loadGroupingModeFromLocalStorage
  mbModuleName <- H.liftEffect getCurrentModuleName
  let currentPackage = getCurrentPackage moduleIndex mbModuleName
  pure $
    H.mkComponent
      { initialState: const
          { packageModules
          , groupingMode
          , moduleNames
          , isIndexHTML
          , currentPackage
          }
      , render
      , eval: H.mkEval $ H.defaultEval
          { handleAction = handleAction
          , handleQuery = handleQuery
          }
      }
  where
  moduleNames =
    Array.sort $ Array.fromFoldable $
      foldr Set.union mempty moduleIndex.packageModules

handleAction
  :: forall o
   . Action
  -> H.HalogenM State Action () o Aff Unit
handleAction (ToggleGrouping groupingMode) = do
  H.modify_ (_groupingMode .~ groupingMode)

  H.liftEffect do
    window <- HTML.window
    localStorage <- Window.localStorage window

    if groupingMode == DontGroup then Storage.setItem Config.groupModulesItem "false" localStorage
    else Storage.removeItem Config.groupModulesItem localStorage

handleQuery
  :: forall a i
   . Query a
  -> H.HalogenM State i () Action Aff (Maybe a)
handleQuery (UpdateModuleGrouping _next) = do
  oldGroupingMode <- H.get <#> _.groupingMode
  newGroupingMode <- H.liftEffect loadGroupingModeFromLocalStorage
  when (oldGroupingMode /= newGroupingMode) do
    H.modify_ (_groupingMode .~ newGroupingMode)
  pure Nothing

render
  :: forall m
   . State
  -> H.ComponentHTML Action () m
render state@{ groupingMode, moduleNames } =
  HH.div
    [ HP.classes
        [ wrap "col"
        , wrap $
            if state.isIndexHTML == IsIndexHTML then "col--main"
            else "col--aside"
        ]
    ]

    [ HH.h3_ [ HH.text $ if groupingMode == DontGroup then "Modules" else "Packages" ]
    , HH.input
        [ HP.id "group-modules__input"
        , HP.type_ HP.InputCheckbox
        , HP.checked (groupingMode == GroupByPackage)
        , HE.onChecked $ ToggleGrouping <<< isCheckedToGroupingMode
        ]

    , HH.text " "
    , HH.label
        [ HP.for "group-modules__input"
        , HP.id "group-modules__label"
        ]
        [ HH.text " GROUP BY PACKAGE" ]

    , HH.ul_ $
        if groupingMode == GroupByPackage then renderPackageEntry <$> packageList
        else renderModuleName <$> moduleNames
    ]
  where

  renderPackageEntry (package /\ modules) =
    HH.li [ HP.classes [ wrap "li-package" ] ]
      [ HH.details
          (if isCurrentPackage then [ HP.attr (wrap "open") "" ] else [])
          [ HH.summary_
              [ HH.text $
                  case package of
                    Package packageName -> PackageName.print packageName
                    LocalPackage packageName -> PackageName.print packageName
                    Builtin -> "<builtins>"
                    UnknownPackage -> "<unknown>"
              ]
          , HH.ul_ $ Set.toUnfoldable modules <#> renderModuleName
          ]
      ]
    where

    isCurrentPackage =
      state.currentPackage == package &&
        -- If we don't know which package we are in, we don't want to expand
        -- "Unknown package" section
        state.currentPackage /= UnknownPackage

  renderModuleName moduleName =
    HH.li_
      [ HH.a [ HP.href (unwrap moduleName <> ".html") ]
          [ HH.text $ unwrap moduleName ]
      ]

  packageList :: Array (PackageInfo /\ Set ModuleName)
  packageList = Map.toUnfoldable state.packageModules

-- | Decide whether to group modules by package in the sidebar, using localStorage.
loadGroupingModeFromLocalStorage :: Effect GroupingMode
loadGroupingModeFromLocalStorage = do
  window <- HTML.window
  localStorage <- Window.localStorage window
  mbDontGroupModules <- Storage.getItem Config.groupModulesItem localStorage
  pure $ if isJust mbDontGroupModules then DontGroup else GroupByPackage

-- | Extract current module name from page title element.
getCurrentModuleName :: Effect (Maybe ModuleName)
getCurrentModuleName = do
  win <- HTML.window
  docPN <- Document.toParentNode <$> HTMLDocument.toDocument <$> Window.document win
  mbPageTitleEl <- ParentNode.querySelector (wrap ".page-title__title") docPN
  map ModuleName <$> traverse (Node.textContent <<< Element.toNode) mbPageTitleEl

-- | Given a module name, get `PackageInfo` for a package this module name belongs to.
getCurrentPackage :: ModuleIndex -> Maybe ModuleName -> PackageInfo
getCurrentPackage { modulePackages } (Just moduleName) =
  fromMaybe UnknownPackage $ Map.lookup moduleName modulePackages
getCurrentPackage { modulePackages: _ } Nothing = UnknownPackage

-- | Convert checkbox status to sidebar mode
isCheckedToGroupingMode :: Boolean -> GroupingMode
isCheckedToGroupingMode = if _ then GroupByPackage else DontGroup

-- Some optics:

_groupingMode :: forall a b rest. (a -> b) -> { groupingMode :: a | rest } -> { groupingMode :: b | rest }
_groupingMode = prop (Proxy :: Proxy "groupingMode")
