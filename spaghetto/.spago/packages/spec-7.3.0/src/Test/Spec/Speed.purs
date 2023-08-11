module Test.Spec.Speed where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Test.Spec.Style (Style)
import Test.Spec.Style as Style

data Speed = Fast | Medium | Slow

derive instance genericSpeed :: Generic Speed _
instance showSpeed :: Show Speed where show = genericShow
instance showEq :: Eq Speed where eq = genericEq

speedOf :: Milliseconds -> Milliseconds -> Speed
speedOf thresh ms | ms > thresh = Slow
speedOf (Milliseconds thresh) (Milliseconds ms) | ms > thresh / 2.0 = Medium
speedOf _ _ = Fast

toStyle :: Speed -> Style
toStyle Fast   = Style.dim
toStyle Medium = Style.yellow
toStyle Slow   = Style.red
