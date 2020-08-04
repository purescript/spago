module Docs.Search.App.Sidebar where

import Docs.Search.Config as Config
import Docs.Search.Meta (Meta)
import Docs.Search.ModuleIndex (PackedModuleIndex)
import Docs.Search.Types (ModuleName, PackageInfo(..), PackageName)

import Prelude

import Data.Array as Array
import Data.Lens ((.~))
import Data.Lens.Record (prop)
import Data.List (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (wrap, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML as HTML
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

type State = { moduleIndex :: Map PackageInfo (Set ModuleName)
             , groupingMode :: GroupingMode
             , moduleNames :: Array ModuleName
             , isIndexHTML :: IsIndexHTML
             , localPackageName :: PackageName
             }


mkComponent
  :: forall i
  .  PackedModuleIndex
  -> IsIndexHTML
  -> Meta
  -> Aff (H.Component HH.HTML Query i Action Aff)
mkComponent moduleIndex isIndexHTML { localPackageName } = do
  groupingMode <- H.liftEffect loadGroupingModeFromLocalStorage
  pure $
    H.mkComponent
      { initialState: const { moduleIndex
                            , groupingMode
                            , moduleNames
                            , isIndexHTML
                            , localPackageName
                            }
      , render
      , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                       , handleQuery = handleQuery
                                       }
      }
  where
    moduleNames = Array.sort $ Array.fromFoldable $ foldr Set.union mempty moduleIndex


handleAction
 :: forall o
 .  Action
 -> H.HalogenM State Action () o Aff Unit
handleAction (ToggleGrouping groupingMode) = do
  H.modify_ (_groupingMode .~ groupingMode)

  H.liftEffect do
    window <- HTML.window
    localStorage <- Window.localStorage window

    if groupingMode == DontGroup
    then Storage.setItem    Config.groupModulesItem "false" localStorage
    else Storage.removeItem Config.groupModulesItem         localStorage


handleQuery
  :: forall a i
  .  Query a
  -> H.HalogenM State i () Action Aff (Maybe a)
handleQuery (UpdateModuleGrouping next) = do
  oldGroupingMode <- H.get <#> _.groupingMode
  newGroupingMode <- H.liftEffect loadGroupingModeFromLocalStorage
  when (oldGroupingMode /= newGroupingMode) do
    H.modify_ (_groupingMode .~ newGroupingMode)
  pure Nothing


render
  :: forall m
  .  State
  -> H.ComponentHTML Action () m
render { moduleIndex, groupingMode, moduleNames, isIndexHTML, localPackageName } =

  HH.div [ HP.classes [ wrap "col"
                      , wrap $ if isIndexHTML == IsIndexHTML
                               then "col--main"
                               else "col--aside"
                      ]
         ]

  [ HH.h3_ [ HH.text $ if groupingMode == DontGroup then "Modules" else "Packages" ]
  , HH.input [ HP.id_ "group-modules__input"
             , HP.type_ HP.InputCheckbox
             , HP.checked (groupingMode == GroupByPackage)
             , HE.onChecked $ Just <<< ToggleGrouping <<< isCheckedToGroupingMode
             ]

  , HH.text " "
  , HH.label [ HP.for "group-modules__input"
             , HP.id_ "group-modules__label"
             ]
    [ HH.text " GROUP BY PACKAGE" ]

  , HH.ul_ $ if groupingMode == GroupByPackage
             then renderPackageEntry <$> packageList
             else renderModuleName <$> moduleNames
  ]
  where

    renderPackageEntry (package /\ modules) =
      HH.li [ HP.classes [ wrap "li-package" ] ]
      [ HH.details_ $
        [ HH.summary_ [ HH.text $
                        case package of
                          Package packageName -> unwrap packageName
                          LocalPackage -> unwrap localPackageName
                          Builtin -> "Built-in"
                          UnknownPackage -> "Unknown package"
                      ]
        , HH.ul_ $ Set.toUnfoldable modules <#> renderModuleName
        ]
      ]

    renderModuleName moduleName =
      HH.li_
      [ HH.a [ HP.href (unwrap moduleName <> ".html") ]
        [ HH.text $ unwrap moduleName ]
      ]

    packageList :: Array (PackageInfo /\ Set ModuleName)
    packageList = Map.toUnfoldable moduleIndex


-- | Decide whether to group modules by package in the sidebar, using localStorage.
loadGroupingModeFromLocalStorage :: Effect GroupingMode
loadGroupingModeFromLocalStorage = do
  window <- HTML.window
  localStorage <- Window.localStorage window
  mbDontGroupModules <- Storage.getItem Config.groupModulesItem localStorage
  pure $ if isJust mbDontGroupModules then DontGroup else GroupByPackage


-- | Convert checkbox status to sidebar mode
isCheckedToGroupingMode :: Boolean -> GroupingMode
isCheckedToGroupingMode = if _ then GroupByPackage else DontGroup


-- Some optics:

_groupingMode :: forall a b rest.  (a -> b) -> { groupingMode :: a | rest } -> { groupingMode :: b | rest }
_groupingMode = prop (SProxy :: SProxy "groupingMode")
