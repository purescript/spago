module Test.Declarations where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Docs.Search.Declarations (extractPackageName)
import Docs.Search.DocTypes (SourceSpan)
import Docs.Search.Types (PackageInfo(..))
import Registry.PackageName (PackageName)
import Spago.Purs.Types as Graph
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

tests :: Spec Unit
tests = do
  describe "Declarations" do
    it "extractPackageName works correctly" do
      Builtin `shouldEqual` (extractPackageName moduleGraph workspacePackages (wrap "Prim"))
      Builtin `shouldEqual` (extractPackageName moduleGraph workspacePackages (wrap "Prim.Foo"))
      Builtin `shouldEqual` (extractPackageName moduleGraph workspacePackages (wrap "Prim.Foo.Bar"))
      UnknownPackage `shouldEqual` (extractPackageName moduleGraph workspacePackages (wrap "Primitive"))
      Package (mkPackageName "foo") `shouldEqual`
        (extractPackageName moduleGraph workspacePackages (wrap "Foo"))
      Package (mkPackageName "bar") `shouldEqual`
        (extractPackageName moduleGraph workspacePackages (wrap "Bar"))
      LocalPackage (mkPackageName "baz") `shouldEqual`
        (extractPackageName moduleGraph workspacePackages (wrap "Baz"))

workspacePackages :: Set PackageName
workspacePackages = Set.fromFoldable [ mkPackageName "baz" ]

moduleGraph :: Graph.ModuleGraphWithPackage
moduleGraph =
  Map.fromFoldable
    [ Tuple "Foo" { depends: [], path: "", package: mkPackageName "foo" }
    , Tuple "Bar" { depends: [], path: "", package: mkPackageName "bar" }
    , Tuple "Baz" { depends: [], path: "", package: mkPackageName "baz" }
    ]

mkSourceSpan :: String -> Maybe SourceSpan
mkSourceSpan name = Just $ wrap { name, start: defaultPos, end: defaultPos }
  where
  defaultPos = wrap { column: 0, line: 0 }

mkPackageName :: String -> PackageName
mkPackageName = unsafeCoerce
