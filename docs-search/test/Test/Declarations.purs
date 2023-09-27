module Test.Declarations where

import Prelude

import Docs.Search.Declarations (extractPackageName)
import Docs.Search.Types (PackageName(..), PackageInfo(..))

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
        (extractPackageName (wrap "Foo") $
         Just { start: []
              , end: []
              , name: ".spago/foo/src/Foo.purs"
              }
        )
      Package (PackageName "bar") `shouldEqual`
        (extractPackageName (wrap "Bar") $
         Just { start: []
              , end: []
              , name: "/path/to/somewhere/bower_components/bar/src/Bar.purs"
              }
        )
      LocalPackage `shouldEqual`
        (extractPackageName (wrap "Bar") $
         Just { start: []
              , end: []
              , name: "/path/to/somewhere/src/Bar.purs"
              }
        )
