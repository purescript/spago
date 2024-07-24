module Test.Spago.Unit.Printer where

import Prelude

import Data.String (joinWith)
import Dodo as Dodo
import Spago.Psa.Output (initialStats)
import Spago.Psa.Printer (renderVerboseStats)
import Test.Prelude (shouldEqual)
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "renderVerboseStats" do
    Spec.it "renders regular stats with all zeroes when there are no errors or warnings" do
      printDoc (renderVerboseStats initialStats) `shouldEqual`
        joinWith "\n"
        [ "           Src   Lib   All"
        , "Warnings     0     0     0"
        , "Errors       0     0     0"
        ]

  where
    printDoc = Dodo.print Dodo.plainText Dodo.twoSpaces
