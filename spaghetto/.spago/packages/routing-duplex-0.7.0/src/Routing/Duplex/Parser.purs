module Routing.Duplex.Parser
  ( RouteError(..)
  , RouteResult(..)
  , RouteParser(..)
  , runRouteParser
  , parsePath
  , run
  , prefix
  , take
  , param
  , flag
  , many1
  , many
  , rest
  , default
  , optional
  , as
  , int
  , boolean
  , hash
  , end
  , module Routing.Duplex.Types
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Control.Lazy (class Lazy)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bitraversable (bitraverse, ltraverse)
import Data.Either (Either(..))
import Data.Foldable (foldl, lookup)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Lazy as Z
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import JSURI (decodeURIComponent)
import Routing.Duplex.Types (RouteParams, RouteState)

data RouteResult a
  = Fail RouteError
  | Success RouteState a

derive instance eqRouteResult :: Eq a => Eq (RouteResult a)
derive instance functorRouteResult :: Functor RouteResult
derive instance genericRouteResult :: Generic (RouteResult a) _

instance showRouteResult :: Show a => Show (RouteResult a) where
  show = genericShow

data RouteError
  = Expected String String
  | ExpectedEndOfPath String
  | MissingParam String
  | MalformedURIComponent String
  | EndOfPath

derive instance eqRouteError :: Eq RouteError
derive instance genericRouteError :: Generic RouteError _

instance showRouteError :: Show RouteError where
  show = genericShow

data RouteParser a
  = Alt (NonEmptyArray (RouteParser a))
  | Chomp (RouteState -> RouteResult a)
  | Prefix String (RouteParser a)

derive instance functorRouteParser :: Functor RouteParser

instance applyRouteParser :: Apply RouteParser where
  apply fx x = Chomp \state ->
    case runRouteParser state fx of
      Fail err -> Fail err
      Success state' f -> f <$> runRouteParser state' x

instance applicativeRouteParser :: Applicative RouteParser where
  pure = Chomp <<< flip Success

instance altRouteParser :: Alt RouteParser where
  alt (Alt ls) (Alt rs) = Alt (ls `altAppend` rs)
  alt (Alt ls) b = Alt (ls `altSnoc` b)
  alt a (Alt rs) = Alt (a `altCons` rs)
  alt (Prefix pre a) (Prefix pre' b)
    | pre == pre' = Prefix pre (a <|> b)
  alt a b = Alt (NEA.cons a (NEA.singleton b))

instance lazyRouteParser :: Lazy (RouteParser a) where
  defer k =
    Chomp \state ->
      runRouteParser state (Z.force parser)
    where
    parser = Z.defer k

altAppend
  :: forall a
   . NonEmptyArray (RouteParser a)
  -> NonEmptyArray (RouteParser a)
  -> NonEmptyArray (RouteParser a)
altAppend ls rs
  | Prefix pre a <- NEA.last ls
  , Prefix pre' b <- NEA.head rs
  , pre == pre' =
      let
        rs' =
          NEA.cons' (Prefix pre (a <|> b)) (NEA.tail rs)
      in
        case NEA.fromArray (NEA.init ls) of
          Just ls' -> ls' `altAppend` rs'
          Nothing -> rs'
  | otherwise = ls <> rs

altCons
  :: forall a
   . RouteParser a
  -> NonEmptyArray (RouteParser a)
  -> NonEmptyArray (RouteParser a)
altCons (Prefix pre a) rs
  | Prefix pre' b <- NEA.head rs
  , pre == pre' =
      NEA.cons' (Prefix pre (a <|> b)) (NEA.tail rs)
altCons a rs = NEA.cons a rs

altSnoc
  :: forall a
   . NonEmptyArray (RouteParser a)
  -> RouteParser a
  -> NonEmptyArray (RouteParser a)
altSnoc ls (Prefix pre b)
  | Prefix pre' a <- NEA.last ls
  , pre == pre' =
      NEA.snoc' (NEA.init ls) (Prefix pre (a <|> b))
altSnoc ls b = NEA.snoc ls b

chompPrefix :: String -> RouteState -> RouteResult Unit
chompPrefix pre state =
  case Array.head state.segments of
    Just pre' | pre == pre' -> Success (state { segments = Array.drop 1 state.segments }) unit
    Just pre' -> Fail $ Expected pre pre'
    _ -> Fail $ EndOfPath

runRouteParser :: forall a. RouteState -> RouteParser a -> RouteResult a
runRouteParser = go
  where
  go state = case _ of
    Alt xs -> foldl (goAlt state) (Fail EndOfPath) xs
    Chomp f -> f state
    Prefix pre p ->
      case chompPrefix pre state of
        Fail err -> Fail err
        Success state' _ -> go state' p

  goAlt state (Fail _) p = runRouteParser state p
  goAlt _ res _ = res

parsePath :: String -> Either RouteError RouteState
parsePath =
  splitAt (flip Tuple "") "#"
    >>> ltraverse splitPath
    >>> map toRouteState
  where
  splitPath =
    splitAt (flip Tuple "") "?"
      >>> bitraverse splitSegments splitParams

  splitSegments = splitNonEmpty (Pattern "/") >>> case _ of
    [ "", "" ] -> Right [ "" ]
    xs -> traverse decodeURIComponent' xs

  splitParams =
    splitNonEmpty (Pattern "&") >>> traverse splitKeyValue

  splitKeyValue =
    splitAt (flip Tuple "") "=" >>> bitraverse decodeURIComponent' decodeURIComponent'

  splitNonEmpty _ "" = []
  splitNonEmpty p s = split p s

  toRouteState (Tuple (Tuple segments params) h) =
    { segments, params, hash: h }

  splitAt k p str =
    case String.indexOf (Pattern p) str of
      Just ix -> Tuple (String.take ix str) (String.drop (ix + String.length p) str)
      Nothing -> k str

  decodeURIComponent' str = case decodeURIComponent str of
    Nothing -> Left (MalformedURIComponent str)
    Just a -> Right a

run :: forall a. RouteParser a -> String -> Either RouteError a
run p = parsePath >=> flip runRouteParser p >>> case _ of
  Fail err -> Left err
  Success _ res -> Right res

prefix :: forall a. String -> RouteParser a -> RouteParser a
prefix = Prefix

take :: RouteParser String
take = Chomp \state ->
  case Array.uncons state.segments of
    Just { head, tail } -> Success (state { segments = tail }) head
    _ -> Fail EndOfPath

param :: String -> RouteParser String
param key = Chomp \state ->
  case lookup key state.params of
    Just a -> Success state a
    _ -> Fail $ MissingParam key

flag :: String -> RouteParser Boolean
flag = default false <<< map (const true) <<< param

many1 :: forall t a. Alt t => Applicative t => RouteParser a -> RouteParser (t a)
many1 p = Chomp go
  where
  go :: RouteState -> RouteResult (t a)
  go state =
    case runRouteParser state p of
      Fail err -> Fail err
      Success state' a -> go' state' (pure a)

  go' :: RouteState -> t a -> RouteResult (t a)
  go' state xs =
    case runRouteParser state p of
      Fail _ -> Success state xs
      Success state' a -> go' state' (xs <|> pure a)

many :: forall t a. Alternative t => RouteParser a -> RouteParser (t a)
many p = many1 p <|> pure empty

rest :: RouteParser (Array String)
rest = Chomp \state -> Success (state { segments = [] }) state.segments

default :: forall a. a -> RouteParser a -> RouteParser a
default = flip (<|>) <<< pure

optional :: forall a. RouteParser a -> RouteParser (Maybe a)
optional = default Nothing <<< map Just

as :: forall a b. (a -> String) -> (a -> Either String b) -> RouteParser a -> RouteParser b
as print decode p = Chomp \state ->
  case runRouteParser state p of
    Fail err -> Fail err
    Success state' a ->
      case decode a of
        Left err -> Fail $ Expected err (print a)
        Right b -> Success state' b

int :: String -> Either String Int
int = maybe (Left "Int") Right <<< Int.fromString

hash :: RouteParser String
hash = Chomp \state -> Success state state.hash

end :: RouteParser Unit
end = Chomp \state ->
  case Array.head state.segments of
    Nothing -> Success state unit
    Just str -> Fail (ExpectedEndOfPath str)

boolean :: String -> Either String Boolean
boolean = case _ of
  "true" -> Right true
  "false" -> Right false
  _ -> Left "Boolean"
