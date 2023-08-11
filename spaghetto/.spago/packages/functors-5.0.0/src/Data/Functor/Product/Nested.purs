module Data.Functor.Product.Nested where

import Prelude

import Data.Const (Const(..))
import Data.Functor.Product (Product(..), product)
import Data.Tuple (Tuple(..))

type Product1 :: forall k. (k -> Type) -> k -> Type
type Product1 a = T2 a (Const Unit)
type Product2 :: forall k. (k -> Type) -> (k -> Type) -> k -> Type
type Product2 a b = T3 a b (Const Unit)
type Product3 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Product3 a b c = T4 a b c (Const Unit)
type Product4 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Product4 a b c d = T5 a b c d (Const Unit)
type Product5 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Product5 a b c d e= T6 a b c d e (Const Unit)
type Product6 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Product6 a b c d e f = T7 a b c d e f (Const Unit)
type Product7 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Product7 a b c d e f g = T8 a b c d e f g (Const Unit)
type Product8 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Product8 a b c d e f g h = T9 a b c d e f g h (Const Unit)
type Product9 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Product9 a b c d e f g h i = T10 a b c d e f g h i (Const Unit)
type Product10 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Product10 a b c d e f g h i j = T11 a b c d e f g h i j (Const Unit)

type T2 :: forall k. (k -> Type) -> (k -> Type) -> k -> Type
type T2 a z = Product a z
type T3 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type T3 a b z = Product a (T2 b z)
type T4 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type T4 a b c z = Product a (T3 b c z)
type T5 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type T5 a b c d z = Product a (T4 b c d z)
type T6 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type T6 a b c d e z = Product a (T5 b c d e z)
type T7 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type T7 a b c d e f z = Product a (T6 b c d e f z)
type T8 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type T8 a b c d e f g z = Product a (T7 b c d e f g z)
type T9 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type T9 a b c d e f g h z = Product a (T8 b c d e f g h z)
type T10 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type T10 a b c d e f g h i z = Product a (T9 b c d e f g h i z)
type T11 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type T11 a b c d e f g h i j z = Product a (T10 b c d e f g h i j z)

infixr 6 product as </\>
infixr 6 type Product as </\>

product1 :: forall a. a ~> Product1 a
product1 a = a </\> Const unit

product2 :: forall a b x. a x -> b x -> Product2 a b x
product2 a b = a </\> b </\> Const unit

product3 :: forall a b c x. a x -> b x -> c x -> Product3 a b c x
product3 a b c = a </\> b </\> c </\> Const unit

product4 :: forall a b c d x. a x -> b x -> c x -> d x -> Product4 a b c d x
product4 a b c d = a </\> b </\> c </\> d </\> Const unit

product5 :: forall a b c d e x. a x -> b x -> c x -> d x -> e x -> Product5 a b c d e x
product5 a b c d e = a </\> b </\> c </\> d </\> e </\> Const unit

product6 :: forall a b c d e f x. a x -> b x -> c x -> d x -> e x -> f x -> Product6 a b c d e f x
product6 a b c d e f = a </\> b </\> c </\> d </\> e </\> f </\> Const unit

product7 :: forall a b c d e f g x. a x -> b x -> c x -> d x -> e x -> f x -> g x -> Product7 a b c d e f g x
product7 a b c d e f g = a </\> b </\> c </\> d </\> e </\> f </\> g </\> Const unit

product8 :: forall a b c d e f g h x. a x -> b x -> c x -> d x -> e x -> f x -> g x -> h x -> Product8 a b c d e f g h x
product8 a b c d e f g h = a </\> b </\> c </\> d </\> e </\> f </\> g </\> h </\> Const unit

product9 :: forall a b c d e f g h i x. a x -> b x -> c x -> d x -> e x -> f x -> g x -> h x -> i x -> Product9 a b c d e f g h i x
product9 a b c d e f g h i = a </\> b </\> c </\> d </\> e </\> f </\> g </\> h </\> i </\> Const unit

product10 :: forall a b c d e f g h i j x. a x -> b x -> c x -> d x -> e x -> f x -> g x -> h x -> i x -> j x -> Product10 a b c d e f g h i j x
product10 a b c d e f g h i j = a </\> b </\> c </\> d </\> e </\> f </\> g </\> h </\> i </\> j </\> Const unit

get1 :: forall a z. T2 a z ~> a
get1 (Product (Tuple a _)) = a

get2 :: forall a b z. T3 a b z ~> b
get2 (Product (Tuple _ (Product (Tuple b _)))) = b

get3 :: forall a b c z. T4 a b c z ~> c
get3 (Product (Tuple _ (Product (Tuple _ (Product (Tuple c _)))))) = c

get4 :: forall a b c d z. T5 a b c d z ~> d
get4 (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple d _)))))))) = d

get5 :: forall a b c d e z. T6 a b c d e z ~> e
get5 (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple e _)))))))))) = e

get6 :: forall a b c d e f z. T7 a b c d e f z ~> f
get6 (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple f _)))))))))))) = f

get7 :: forall a b c d e f g z. T8 a b c d e f g z ~> g
get7 (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple g _)))))))))))))) = g

get8 :: forall a b c d e f g h z. T9 a b c d e f g h z ~> h
get8 (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple h _)))))))))))))))) = h

get9 :: forall a b c d e f g h i z. T10 a b c d e f g h i z ~> i
get9 (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple i _)))))))))))))))))) = i

get10 :: forall a b c d e f g h i j z. T11 a b c d e f g h i j z ~> j
get10 (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple _ (Product (Tuple j _)))))))))))))))))))) = j
