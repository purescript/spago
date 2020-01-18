module Test.Declarations where

import Prelude

import Data.Maybe (Maybe(..))
import Docs.Search.Declarations (extractPackageName)

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  suite "Declarations" do
    test "extractPackageName" do
      Assert.equal "<builtin>" (extractPackageName "Prim" Nothing)
      Assert.equal "<builtin>" (extractPackageName "Prim.Foo" Nothing)
      Assert.equal "<builtin>" (extractPackageName "Prim.Foo.Bar" Nothing)
      Assert.equal "<unknown>" (extractPackageName "Primitive" Nothing)
      Assert.equal "foo"
        (extractPackageName "Foo" $
         Just { start: []
              , end: []
              , name: ".spago/foo/src/Foo.purs"
              }
        )
      Assert.equal "bar"
        (extractPackageName "Bar" $
         Just { start: []
              , end: []
              , name: "/path/to/somewhere/bower_components/bar/src/Bar.purs"
              }
        )
      Assert.equal "<local package>"
        (extractPackageName "Bar" $
         Just { start: []
              , end: []
              , name: "/path/to/somewhere/src/Bar.purs"
              }
        )
