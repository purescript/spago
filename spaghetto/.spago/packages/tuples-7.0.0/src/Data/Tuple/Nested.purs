-- | Tuples that are not restricted to two elements.
-- |
-- | Here is an example of a 3-tuple:
-- |
-- | 
-- | ```purescript
-- | > tuple = tuple3 1 "2" 3.0
-- | > tuple
-- | (Tuple 1 (Tuple "2" (Tuple 3.0 unit)))
-- | ```
-- | 
-- | Notice that a tuple is a nested structure not unlike a list. The type of `tuple` is this:
-- | 
-- | ```purescript
-- | > :t tuple
-- | Tuple Int (Tuple String (Tuple Number Unit))
-- | ```
-- | 
-- | That, however, can be abbreviated with the `Tuple3` type:
-- | 
-- | ```purescript
-- | Tuple3 Int String Number
-- | ```
-- |
-- | All tuple functions are numbered from 1 to 10. That is, there's
-- | a `get1` and a `get10`.
-- |
-- | The `getN` functions accept tuples of length N or greater:
-- | 
-- | ```purescript
-- | get1 tuple = 1
-- | get3 tuple = 3
-- | get4 tuple -- type error. `get4` requires a longer tuple. 
-- | ```
-- | 
-- | The same is true of the `overN` functions:
-- | 
-- | ```purescript
-- | over2 negate (tuple3 1 2 3) = tuple3 1 (-2) 3
-- | ```
-- |

-- | `uncurryN` can be used to convert a function that takes `N` arguments to one that takes an N-tuple:
-- | 
-- | ```purescript
-- | uncurry2 (+) (tuple2 1 2) = 3
-- | ```
-- | 
-- | The reverse `curryN` function converts functions that take
-- | N-tuples (which are rare) to functions that take `N` arguments.
-- |
-- | ---------------
-- | In addition to types like `Tuple3`, there are also types like
-- | `T3`. Whereas `Tuple3` describes a tuple with exactly three
-- | elements, `T3` describes a tuple of length *two or longer*. More
-- | specifically, `T3` requires two element plus a "tail" that may be
-- | `unit` or more tuple elements. Use types like `T3` when you want to
-- | create a set of functions for arbitrary tuples. See the source for how that's done.
-- | 
module Data.Tuple.Nested where

import Prelude
import Data.Tuple (Tuple(..))

-- | Shorthand for constructing n-tuples as nested pairs.
-- | `a /\ b /\ c /\ d /\ unit` becomes `Tuple a (Tuple b (Tuple c (Tuple d unit)))`
infixr 6 Tuple as /\

-- | Shorthand for constructing n-tuple types as nested pairs.
-- | `forall a b c d. a /\ b /\ c /\ d /\ Unit` becomes
-- | `forall a b c d. Tuple a (Tuple b (Tuple c (Tuple d Unit)))`
infixr 6 type Tuple as /\

type Tuple1 a = T2 a Unit
type Tuple2 a b = T3 a b Unit
type Tuple3 a b c = T4 a b c Unit
type Tuple4 a b c d = T5 a b c d Unit
type Tuple5 a b c d e= T6 a b c d e Unit
type Tuple6 a b c d e f = T7 a b c d e f Unit
type Tuple7 a b c d e f g = T8 a b c d e f g Unit
type Tuple8 a b c d e f g h = T9 a b c d e f g h Unit
type Tuple9 a b c d e f g h i = T10 a b c d e f g h i Unit
type Tuple10 a b c d e f g h i j = T11 a b c d e f g h i j Unit

type T2 a z = Tuple a z
type T3 a b z = Tuple a (T2 b z)
type T4 a b c z = Tuple a (T3 b c z)
type T5 a b c d z = Tuple a (T4 b c d z)
type T6 a b c d e z = Tuple a (T5 b c d e z)
type T7 a b c d e f z = Tuple a (T6 b c d e f z)
type T8 a b c d e f g z = Tuple a (T7 b c d e f g z)
type T9 a b c d e f g h z = Tuple a (T8 b c d e f g h z)
type T10 a b c d e f g h i z = Tuple a (T9 b c d e f g h i z)
type T11 a b c d e f g h i j z = Tuple a (T10 b c d e f g h i j z)

-- | Creates a singleton tuple.
tuple1 :: forall a. a -> Tuple1 a
tuple1 a = a /\ unit

-- | Given 2 values, creates a 2-tuple.
tuple2 :: forall a b. a -> b -> Tuple2 a b
tuple2 a b = a /\ b /\ unit

-- | Given 3 values, creates a nested 3-tuple.
tuple3 :: forall a b c. a -> b -> c -> Tuple3 a b c
tuple3 a b c = a /\ b /\ c /\ unit

-- | Given 4 values, creates a nested 4-tuple.
tuple4 :: forall a b c d. a -> b -> c -> d -> Tuple4 a b c d
tuple4 a b c d = a /\ b /\ c /\ d /\ unit

-- | Given 5 values, creates a nested 5-tuple.
tuple5 :: forall a b c d e. a -> b -> c -> d -> e -> Tuple5 a b c d e
tuple5 a b c d e = a /\ b /\ c /\ d /\ e /\ unit

-- | Given 6 values, creates a nested 6-tuple.
tuple6 :: forall a b c d e f. a -> b -> c -> d -> e -> f -> Tuple6 a b c d e f
tuple6 a b c d e f = a /\ b /\ c /\ d /\ e /\ f /\ unit

-- | Given 7 values, creates a nested 7-tuple.
tuple7 :: forall a b c d e f g. a -> b -> c -> d -> e -> f -> g -> Tuple7 a b c d e f g
tuple7 a b c d e f g = a /\ b /\ c /\ d /\ e /\ f /\ g /\ unit

-- | Given 8 values, creates a nested 8-tuple.
tuple8 :: forall a b c d e f g h. a -> b -> c -> d -> e -> f -> g -> h -> Tuple8 a b c d e f g h
tuple8 a b c d e f g h = a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ unit

-- | Given 9 values, creates a nested 9-tuple.
tuple9 :: forall a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> Tuple9 a b c d e f g h i
tuple9 a b c d e f g h i = a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i /\ unit

-- | Given 10 values, creates a nested 10-tuple.
tuple10 :: forall a b c d e f g h i j. a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Tuple10 a b c d e f g h i j
tuple10 a b c d e f g h i j = a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i /\ j /\ unit

-- | Given at least a singleton tuple, gets the first value.
get1 :: forall a z. T2 a z -> a
get1 (a /\ _) = a

-- | Given at least a 2-tuple, gets the second value.
get2 :: forall a b z. T3 a b z -> b
get2 (_ /\ b /\ _) = b

-- | Given at least a 3-tuple, gets the third value.
get3 :: forall a b c z. T4 a b c z -> c
get3 (_ /\ _ /\ c /\ _) = c

-- | Given at least a 4-tuple, gets the fourth value.
get4 :: forall a b c d z. T5 a b c d z -> d
get4 (_ /\ _ /\ _ /\ d /\ _) = d

-- | Given at least a 5-tuple, gets the fifth value.
get5 :: forall a b c d e z. T6 a b c d e z -> e
get5 (_ /\ _ /\ _ /\ _ /\ e /\ _) = e

-- | Given at least a 6-tuple, gets the sixth value.
get6 :: forall a b c d e f z. T7 a b c d e f z -> f
get6 (_ /\ _ /\ _ /\ _ /\ _ /\ f /\ _) = f

-- | Given at least a 7-tuple, gets the seventh value.
get7 :: forall a b c d e f g z. T8 a b c d e f g z -> g
get7 (_ /\ _ /\ _ /\ _ /\ _ /\ _ /\ g /\ _) = g

-- | Given at least an 8-tuple, gets the eigth value.
get8 :: forall a b c d e f g h z. T9 a b c d e f g h z -> h
get8 (_ /\ _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ h /\ _) = h

-- | Given at least a 9-tuple, gets the ninth value.
get9 :: forall a b c d e f g h i z. T10 a b c d e f g h i z -> i
get9 (_ /\ _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ i /\ _) = i

-- | Given at least a 10-tuple, gets the tenth value.
get10 :: forall a b c d e f g h i j z. T11 a b c d e f g h i j z -> j
get10 (_ /\ _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ j /\ _) = j

-- | Given at least a singleton tuple, modifies the first value.
over1 :: forall a r z. (a -> r) -> T2 a z -> T2 r z
over1 o (a /\ z) = o a /\ z

-- | Given at least a 2-tuple, modifies the second value.
over2 :: forall a b r z. (b -> r) -> T3 a b z -> T3 a r z
over2 o (a /\ b /\ z) = a /\ o b /\ z

-- | Given at least a 3-tuple, modifies the third value.
over3 :: forall a b c r z. (c -> r) -> T4 a b c z -> T4 a b r z
over3 o (a /\ b /\ c /\ z) = a /\ b /\ o c /\ z

-- | Given at least a 4-tuple, modifies the fourth value.
over4 :: forall a b c d r z. (d -> r) -> T5 a b c d z -> T5 a b c r z
over4 o (a /\ b /\ c /\ d /\ z) = a /\ b /\ c /\ o d /\ z

-- | Given at least a 5-tuple, modifies the fifth value.
over5 :: forall a b c d e r z. (e -> r) -> T6 a b c d e z -> T6 a b c d r z
over5 o (a /\ b /\ c /\ d /\ e /\ z) = a /\ b /\ c /\ d /\ o e /\ z

-- | Given at least a 6-tuple, modifies the sixth value.
over6 :: forall a b c d e f r z. (f -> r) -> T7 a b c d e f z -> T7 a b c d e r z
over6 o (a /\ b /\ c /\ d /\ e /\ f /\ z) = a /\ b /\ c /\ d /\ e /\ o f /\ z

-- | Given at least a 7-tuple, modifies the seventh value.
over7 :: forall a b c d e f g r z. (g -> r) -> T8 a b c d e f g z -> T8 a b c d e f r z
over7 o (a /\ b /\ c /\ d /\ e /\ f /\ g /\ z) = a /\ b /\ c /\ d /\ e /\ f /\ o g /\ z

-- | Given at least an 8-tuple, modifies the eighth value.
over8 :: forall a b c d e f g h r z. (h -> r) -> T9 a b c d e f g h z -> T9 a b c d e f g r z
over8 o (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ z) = a /\ b /\ c /\ d /\ e /\ f /\ g /\ o h /\ z

-- | Given at least a 9-tuple, modifies the ninth value.
over9 :: forall a b c d e f g h i r z. (i -> r) -> T10 a b c d e f g h i z -> T10 a b c d e f g h r z
over9 o (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i /\ z) = a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ o i /\ z

-- | Given at least a 10-tuple, modifies the tenth value.
over10 :: forall a b c d e f g h i j r z. (j -> r) -> T11 a b c d e f g h i j z -> T11 a b c d e f g h i r z
over10 o (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i /\ j /\ z) = a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i /\ o j /\ z

-- | Given a function of 1 argument, returns a function that accepts a singleton tuple.
uncurry1 :: forall a r z. (a -> r) -> T2 a z -> r
uncurry1 f (a /\ _) = f a

-- | Given a function of 2 arguments, returns a function that accepts a 2-tuple.
uncurry2 :: forall a b r z. (a -> b -> r) -> T3 a b z -> r
uncurry2 f (a /\ b /\ _) = f a b

-- | Given a function of 3 arguments, returns a function that accepts a 3-tuple.
uncurry3 :: forall a b c r z. (a -> b -> c -> r) -> T4 a b c z -> r
uncurry3 f (a /\ b /\ c /\ _) = f a b c

-- | Given a function of 4 arguments, returns a function that accepts a 4-tuple.
uncurry4 :: forall a b c d r z. (a -> b -> c -> d -> r) -> T5 a b c d z -> r
uncurry4 f (a /\ b /\ c /\ d /\ _) = f a b c d

-- | Given a function of 5 arguments, returns a function that accepts a 5-tuple.
uncurry5 :: forall a b c d e r z. (a -> b -> c -> d -> e -> r) -> T6 a b c d e z -> r
uncurry5 f (a /\ b /\ c /\ d /\ e /\ _) = f a b c d e

-- | Given a function of 6 arguments, returns a function that accepts a 6-tuple.
uncurry6 :: forall a b c d e f r z. (a -> b -> c -> d -> e -> f -> r) -> T7 a b c d e f z -> r
uncurry6 f' (a /\ b /\ c /\ d /\ e /\ f /\ _) = f' a b c d e f

-- | Given a function of 7 arguments, returns a function that accepts a 7-tuple.
uncurry7 :: forall a b c d e f g r z. (a -> b -> c -> d -> e -> f -> g -> r) -> T8 a b c d e f g z -> r
uncurry7 f' (a /\ b /\ c /\ d /\ e /\ f /\ g /\ _) = f' a b c d e f g

-- | Given a function of 8 arguments, returns a function that accepts an 8-tuple.
uncurry8 :: forall a b c d e f g h r z. (a -> b -> c -> d -> e -> f -> g -> h -> r) -> T9 a b c d e f g h z -> r
uncurry8 f' (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ _) = f' a b c d e f g h

-- | Given a function of 9 arguments, returns a function that accepts a 9-tuple.
uncurry9 :: forall a b c d e f g h i r z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> r) -> T10 a b c d e f g h i z -> r
uncurry9 f' (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i /\ _) = f' a b c d e f g h i

-- | Given a function of 10 arguments, returns a function that accepts a 10-tuple.
uncurry10 :: forall a b c d e f g h i j r z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> r) -> T11 a b c d e f g h i j z -> r
uncurry10 f' (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i /\ j /\ _) = f' a b c d e f g h i j

-- | Given a function that accepts at least a singleton tuple, returns a function of 1 argument.
curry1 :: forall a r z. z -> (T2 a z -> r) -> a -> r
curry1 z f a = f (a /\ z)

-- | Given a function that accepts at least a 2-tuple, returns a function of 2 arguments.
curry2 :: forall a b r z. z -> (T3 a b z -> r) -> a -> b -> r
curry2 z f a b = f (a /\ b /\ z)

-- | Given a function that accepts at least a 3-tuple, returns a function of 3 arguments.
curry3 :: forall a b c r z. z -> (T4 a b c z -> r) -> a -> b -> c -> r
curry3 z f a b c = f (a /\ b /\ c /\ z)

-- | Given a function that accepts at least a 4-tuple, returns a function of 4 arguments.
curry4 :: forall a b c d r z. z -> (T5 a b c d z -> r) -> a -> b -> c -> d -> r
curry4 z f a b c d = f (a /\ b /\ c /\ d /\ z)

-- | Given a function that accepts at least a 5-tuple, returns a function of 5 arguments.
curry5 :: forall a b c d e r z. z -> (T6 a b c d e z -> r) -> a -> b -> c -> d -> e -> r
curry5 z f a b c d e = f (a /\ b /\ c /\ d /\ e /\ z)

-- | Given a function that accepts at least a 6-tuple, returns a function of 6 arguments.
curry6 :: forall a b c d e f r z. z -> (T7 a b c d e f z -> r) -> a -> b -> c -> d -> e -> f -> r
curry6 z f' a b c d e f = f' (a /\ b /\ c /\ d /\ e /\ f /\ z)

-- | Given a function that accepts at least a 7-tuple, returns a function of 7 arguments.
curry7 :: forall a b c d e f g r z. z -> (T8 a b c d e f g z -> r) -> a -> b -> c -> d -> e -> f -> g -> r
curry7 z f' a b c d e f g = f' (a /\ b /\ c /\ d /\ e /\ f /\ g /\ z)

-- | Given a function that accepts at least an 8-tuple, returns a function of 8 arguments.
curry8 :: forall a b c d e f g h r z. z -> (T9 a b c d e f g h z -> r) -> a -> b -> c -> d -> e -> f -> g -> h -> r
curry8 z f' a b c d e f g h = f' (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ z)

-- | Given a function that accepts at least a 9-tuple, returns a function of 9 arguments.
curry9 :: forall a b c d e f g h i r z. z -> (T10 a b c d e f g h i z -> r) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> r
curry9 z f' a b c d e f g h i = f' (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i /\ z)

-- | Given a function that accepts at least a 10-tuple, returns a function of 10 arguments.
curry10 :: forall a b c d e f g h i j r z. z -> (T11 a b c d e f g h i j z -> r) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> r
curry10 z f' a b c d e f g h i j = f' (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i /\ j /\ z)
