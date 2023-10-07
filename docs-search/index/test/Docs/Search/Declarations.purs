module Test.Declarations where

import Prelude

import Docs.Search.Declarations (extractPackageName)
import Docs.Search.Types (PackageName(..), PackageInfo(..))
import Docs.Search.DocTypes (SourceSpan)

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  describe "Declarations" do
    it "extractPackageName works correctly" do
      Builtin `shouldEqual` (extractPackageName (wrap "Prim") Nothing)
      Builtin `shouldEqual` (extractPackageName (wrap "Prim.Foo") Nothing)
      Builtin `shouldEqual` (extractPackageName (wrap "Prim.Foo.Bar") Nothing)
      UnknownPackage `shouldEqual` (extractPackageName (wrap "Primitive") Nothing)
      Package (PackageName "foo") `shouldEqual`
        ( extractPackageName (wrap "Foo") $
            mkSourceSpan ".spago/foo/src/Foo.purs"
        )
      Package (PackageName "bar") `shouldEqual`
        ( extractPackageName (wrap "Bar") $
            mkSourceSpan "/path/to/somewhere/bower_components/bar/src/Bar.purs"
        )
      LocalPackage `shouldEqual`
        ( extractPackageName (wrap "Bar") $
            mkSourceSpan "/path/to/somewhere/src/Bar.purs"
        )

mkSourceSpan :: String -> Maybe SourceSpan
mkSourceSpan name = Just $ wrap { name, start: defaultPos, end: defaultPos }
  where
  defaultPos = wrap { column: 0, line: 0 }
