module Test.Declarations where

import Prelude

import Docs.Search.Declarations (extractPackageName)
import Docs.Search.Types (PackageName(..), PackageInfo(..))

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  suite "Declarations" do
    test "extractPackageName" do
      Assert.equal Builtin (extractPackageName (wrap "Prim") Nothing)
      Assert.equal Builtin (extractPackageName (wrap "Prim.Foo") Nothing)
      Assert.equal Builtin (extractPackageName (wrap "Prim.Foo.Bar") Nothing)
      Assert.equal UnknownPackage (extractPackageName (wrap "Primitive") Nothing)
      Assert.equal (Package $ PackageName "foo")
        (extractPackageName (wrap "Foo") $
         Just { start: []
              , end: []
              , name: ".spago/foo/src/Foo.purs"
              }
        )
      Assert.equal (Package $ PackageName "bar")
        (extractPackageName (wrap "Bar") $
         Just { start: []
              , end: []
              , name: "/path/to/somewhere/bower_components/bar/src/Bar.purs"
              }
        )
      Assert.equal LocalPackage
        (extractPackageName (wrap "Bar") $
         Just { start: []
              , end: []
              , name: "/path/to/somewhere/src/Bar.purs"
              }
        )
