module Routing.Duplex.Printer
  ( RoutePrinter(..)
  , put
  , param
  , flag
  , hash
  , run
  , printPath
  ) where

import Prelude

import Data.Array as Array
import Data.Function (applyFlipped)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), uncurry)
import JSURI (encodeURIComponent)
import Routing.Duplex.Types (RouteState, emptyRouteState)

newtype RoutePrinter = RoutePrinter (RouteState -> RouteState)

derive instance newtypeRoutePrinter :: Newtype RoutePrinter _

instance semigroupRoutePrinter :: Semigroup RoutePrinter where
  append (RoutePrinter f) (RoutePrinter g) = RoutePrinter (f >>> g)

instance monoidRoutePRinter :: Monoid RoutePrinter where
  mempty = RoutePrinter identity

put :: String -> RoutePrinter
put str = RoutePrinter \state -> state { segments = Array.snoc state.segments str }

param :: String -> String -> RoutePrinter
param key val = RoutePrinter \state -> state { params = Array.cons (Tuple key val) state.params }

flag :: String -> Boolean -> RoutePrinter
flag key val
  | val = param key ""
  | otherwise = mempty

hash :: String -> RoutePrinter
hash h = RoutePrinter _ { hash = h }

run :: RoutePrinter -> String
run = printPath <<< applyFlipped emptyRouteState <<< unwrap

printPath :: RouteState -> String
printPath { segments, params, hash: hash' } =
  printSegments segments <> printParams params <> printHash hash'
  where
  printSegments = case _ of
    [ "" ] -> "/"
    xs -> joinWith "/" $ Array.mapMaybe encodeURIComponent xs

  printParams [] = ""
  printParams ps = "?" <> joinWith "&" (Array.mapMaybe (uncurry printParam) ps)

  printParam key "" = encodeURIComponent key
  printParam key val = encodeURIComponent key <> Just "=" <> encodeURIComponent val

  printHash "" = ""
  printHash h = "#" <> h
