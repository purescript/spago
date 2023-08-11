module Routing.Duplex.Generic.Syntax where

import Prelude

import Data.Generic.Rep (Argument, Product)
import Routing.Duplex (class RouteDuplexParams, RouteDuplex, RouteDuplex', params, prefix, suffix)
import Routing.Duplex.Generic (class GRouteDuplexCtr, gRouteDuplexCtr, product)

class GSep a b c | a b -> c where
  gsep :: a -> b -> RouteDuplex' c

instance gsepStringString ::
  GSep String String Unit where
  gsep a b = prefix a $ prefix b $ pure unit
else instance gsepStringRoute ::
  GRouteDuplexCtr a b =>
  GSep String (RouteDuplex a a) b where
  gsep a = prefix a <<< gRouteDuplexCtr
else instance gsepRouteString ::
  GRouteDuplexCtr a b =>
  GSep (RouteDuplex a a) String b where
  gsep = suffix <<< gRouteDuplexCtr
else instance gsepProduct ::
  GRouteDuplexCtr b c =>
  GSep (RouteDuplex a a) (RouteDuplex b b) (Product (Argument a) c) where
  gsep = product

infixr 1 gsep as /

class GParams a b c | a b -> c where
  gparams :: a -> b -> RouteDuplex' c

instance gparamsString ::
  RouteDuplexParams r1 r2 =>
  GParams String { | r1 } { | r2 } where
  gparams a = prefix a <<< params
else instance gparamsRoute ::
  RouteDuplexParams r1 r2 =>
  GParams (RouteDuplex a a) { | r1 } (Product (Argument a) (Argument { | r2 })) where
  gparams a = product a <<< params

infix 8 gparams as ?
