module Data.Interval.Duration.Iso
  ( IsoDuration
  , unIsoDuration
  , mkIsoDuration
  , Error(..)
  , Errors
  , prettyError
  ) where

import Prelude

import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Interval.Duration (Duration(..), DurationComponent(..))
import Data.List (List(..), reverse, span, null)
import Data.List.NonEmpty (fromList)
import Data.List.Types (NonEmptyList)
import Data.Map as Map
import Data.Number as Number
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), snd)

newtype IsoDuration = IsoDuration Duration

derive instance eqIsoDuration :: Eq IsoDuration
derive instance ordIsoDuration :: Ord IsoDuration
instance showIsoDuration :: Show IsoDuration where
  show (IsoDuration d) = "(IsoDuration " <> show d <> ")"

type Errors = NonEmptyList Error

data Error
  = IsEmpty
  | InvalidWeekComponentUsage
  | ContainsNegativeValue DurationComponent
  | InvalidFractionalUse DurationComponent

derive instance eqError :: Eq Error
derive instance ordError :: Ord Error
instance showError :: Show Error where
  show (IsEmpty) = "(IsEmpty)"
  show (InvalidWeekComponentUsage) = "(InvalidWeekComponentUsage)"
  show (ContainsNegativeValue c) = "(ContainsNegativeValue " <> show c <> ")"
  show (InvalidFractionalUse c) = "(InvalidFractionalUse " <> show c <> ")"

prettyError :: Error -> String
prettyError (IsEmpty) = "Duration is empty (has no components)"
prettyError (InvalidWeekComponentUsage) = "Week component of Duration is used with other components"
prettyError (ContainsNegativeValue c) = "Component `" <> show c <> "` contains negative value"
prettyError (InvalidFractionalUse c) = "Invalid usage of Fractional value at component `" <> show c <> "`"


unIsoDuration :: IsoDuration -> Duration
unIsoDuration (IsoDuration a) = a

mkIsoDuration :: Duration -> Either Errors IsoDuration
mkIsoDuration d = case fromList (checkValidIsoDuration d) of
  Just errs -> Left errs
  Nothing -> Right (IsoDuration d)

checkValidIsoDuration :: Duration -> List Error
checkValidIsoDuration (Duration asMap) = check {asList, asMap}
  where
  asList = reverse (Map.toUnfoldable asMap)
  check = fold
    [ checkWeekUsage
    , checkEmptiness
    , checkFractionalUse
    , checkNegativeValues
    ]


type CheckEnv =
  { asList :: List (Tuple DurationComponent Number)
  , asMap :: Map.Map DurationComponent Number}

checkWeekUsage :: CheckEnv -> List Error
checkWeekUsage {asMap} = if isJust (Map.lookup Week asMap) && Map.size asMap > 1
  then pure InvalidWeekComponentUsage else empty

checkEmptiness :: CheckEnv -> List Error
checkEmptiness {asList} = if null asList then pure IsEmpty else empty

checkFractionalUse :: CheckEnv -> List Error
checkFractionalUse {asList} = case _.rest (span (snd >>> not isFractional) asList) of
  Cons (Tuple c _) rest | checkRest rest -> pure (InvalidFractionalUse c)
  _ -> empty
  where
  isFractional a = Number.floor a /= a
  checkRest rest = unwrap (foldMap (snd >>> Number.abs >>> Additive) rest) > 0.0

checkNegativeValues :: CheckEnv -> List Error
checkNegativeValues {asList} = flip foldMap asList \(Tuple c num) ->
  if num >= 0.0 then empty else pure (ContainsNegativeValue c)
