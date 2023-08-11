module Data.Functor.Coproduct.Nested where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(..), coproduct, left, right)
import Data.Newtype (unwrap)

type Coproduct1 :: forall k. (k -> Type) -> k -> Type
type Coproduct1 a = C2 a (Const Void)
type Coproduct2 :: forall k. (k -> Type) -> (k -> Type) -> k -> Type
type Coproduct2 a b = C3 a b (Const Void)
type Coproduct3 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Coproduct3 a b c = C4 a b c (Const Void)
type Coproduct4 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Coproduct4 a b c d = C5 a b c d (Const Void)
type Coproduct5 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Coproduct5 a b c d e = C6 a b c d e (Const Void)
type Coproduct6 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Coproduct6 a b c d e f = C7 a b c d e f (Const Void)
type Coproduct7 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Coproduct7 a b c d e f g = C8 a b c d e f g (Const Void)
type Coproduct8 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Coproduct8 a b c d e f g h = C9 a b c d e f g h (Const Void)
type Coproduct9 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Coproduct9 a b c d e f g h i = C10 a b c d e f g h i (Const Void)
type Coproduct10 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type Coproduct10 a b c d e f g h i j = C11 a b c d e f g h i j (Const Void)

type C2 :: forall k. (k -> Type) -> (k -> Type) -> k -> Type
type C2 a z = Coproduct a z
type C3 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type C3 a b z = Coproduct a (C2 b z)
type C4 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type C4 a b c z = Coproduct a (C3 b c z)
type C5 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type C5 a b c d z = Coproduct a (C4 b c d z)
type C6 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type C6 a b c d e z = Coproduct a (C5 b c d e z)
type C7 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type C7 a b c d e f z = Coproduct a (C6 b c d e f z)
type C8 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type C8 a b c d e f g z = Coproduct a (C7 b c d e f g z)
type C9 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type C9 a b c d e f g h z = Coproduct a (C8 b c d e f g h z)
type C10 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type C10 a b c d e f g h i z = Coproduct a (C9 b c d e f g h i z)
type C11 :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
type C11 a b c d e f g h i j z = Coproduct a (C10 b c d e f g h i j z)

infixr 6 coproduct as <\/>
infixr 6 type Coproduct as <\/>

in1 :: forall a z. a ~> C2 a z
in1 = left

in2 :: forall a b z. b ~> C3 a b z
in2 v = right (left v)

in3 :: forall a b c z. c ~> C4 a b c z
in3 v = right (right (left v))

in4 :: forall a b c d z. d ~> C5 a b c d z
in4 v = right (right (right (left v)))

in5 :: forall a b c d e z. e ~> C6 a b c d e z
in5 v = right (right (right (right (left v))))

in6 :: forall a b c d e f z. f ~> C7 a b c d e f z
in6 v = right (right (right (right (right (left v)))))

in7 :: forall a b c d e f g z. g ~> C8 a b c d e f g z
in7 v = right (right (right (right (right (right (left v))))))

in8 :: forall a b c d e f g h z. h ~> C9 a b c d e f g h z
in8 v = right (right (right (right (right (right (right (left v)))))))

in9 :: forall a b c d e f g h i z. i ~> C10 a b c d e f g h i z
in9 v = right (right (right (right (right (right (right (right (left v))))))))

in10 :: forall a b c d e f g h i j z. j ~> C11 a b c d e f g h i j z
in10 v = right (right (right (right (right (right (right (right (right (left v)))))))))

at1 :: forall r x a z. r -> (a x -> r) -> C2 a z x -> r
at1 b f y = case y of
  Coproduct (Left r) -> f r
  _ -> b

at2 :: forall r x a b z. r -> (b x -> r) -> C3 a b z x -> r
at2 b f y = case y of
  Coproduct (Right (Coproduct (Left r))) -> f r
  _ -> b

at3 :: forall r x a b c z. r -> (c x -> r) -> C4 a b c z x -> r
at3 b f y = case y of
  Coproduct (Right (Coproduct (Right (Coproduct (Left r))))) -> f r
  _ -> b

at4 :: forall r x a b c d z. r -> (d x -> r) -> C5 a b c d z x -> r
at4 b f y = case y of
  Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Left r))))))) -> f r
  _ -> b

at5 :: forall r x a b c d e z. r -> (e x -> r) -> C6 a b c d e z x -> r
at5 b f y = case y of
  Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Left r))))))))) -> f r
  _ -> b

at6 :: forall r x a b c d e f z. r -> (f x -> r) -> C7 a b c d e f z x -> r
at6 b f y = case y of
  Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Left r))))))))))) -> f r
  _ -> b

at7 :: forall r x a b c d e f g z. r -> (g x -> r) -> C8 a b c d e f g z x -> r
at7 b f y = case y of
  Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Left r))))))))))))) -> f r
  _ -> b

at8 :: forall r x a b c d e f g h z. r -> (h x -> r) -> C9 a b c d e f g h z x -> r
at8 b f y = case y of
  Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Left r))))))))))))))) -> f r
  _ -> b

at9 :: forall r x a b c d e f g h i z. r -> (i x -> r) -> C10 a b c d e f g h i z x -> r
at9 b f y = case y of
  Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Left r))))))))))))))))) -> f r
  _ -> b

at10 :: forall r x a b c d e f g h i j z. r -> (j x -> r) -> C11 a b c d e f g h i j z x -> r
at10 b f y = case y of
  Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Right (Coproduct (Left r))))))))))))))))))) -> f r
  _ -> b

coproduct1 :: forall a. Coproduct1 a ~> a
coproduct1 y = case y of
  Coproduct (Left r) -> r
  Coproduct (Right _1) -> absurd (unwrap _1)

coproduct2 :: forall r x a b. (a x -> r) -> (b x -> r) -> Coproduct2 a b x -> r
coproduct2 a b y = case y of
  Coproduct (Left r) -> a r
  Coproduct (Right _1) -> case _1 of
    Coproduct (Left r) -> b r
    Coproduct (Right _2) -> absurd (unwrap _2)

coproduct3 :: forall r x a b c. (a x -> r) -> (b x -> r) -> (c x -> r) -> Coproduct3 a b c x -> r
coproduct3 a b c y = case y of
  Coproduct (Left r) -> a r
  Coproduct (Right _1) -> case _1 of
    Coproduct (Left r) -> b r
    Coproduct (Right _2) -> case _2 of
      Coproduct (Left r) -> c r
      Coproduct (Right _3) -> absurd (unwrap _3)

coproduct4 :: forall r x a b c d. (a x -> r) -> (b x -> r) -> (c x -> r) -> (d x -> r) -> Coproduct4 a b c d x -> r
coproduct4 a b c d y = case y of
  Coproduct (Left r) -> a r
  Coproduct (Right _1) -> case _1 of
    Coproduct (Left r) -> b r
    Coproduct (Right _2) -> case _2 of
      Coproduct (Left r) -> c r
      Coproduct (Right _3) -> case _3 of
        Coproduct (Left r) -> d r
        Coproduct (Right _4) -> absurd (unwrap _4)

coproduct5 :: forall r x a b c d e. (a x -> r) -> (b x -> r) -> (c x -> r) -> (d x -> r) -> (e x -> r) -> Coproduct5 a b c d e x -> r
coproduct5 a b c d e y = case y of
  Coproduct (Left r) -> a r
  Coproduct (Right _1) -> case _1 of
    Coproduct (Left r) -> b r
    Coproduct (Right _2) -> case _2 of
      Coproduct (Left r) -> c r
      Coproduct (Right _3) -> case _3 of
        Coproduct (Left r) -> d r
        Coproduct (Right _4) -> case _4 of
          Coproduct (Left r) -> e r
          Coproduct (Right _5) -> absurd (unwrap _5)

coproduct6 :: forall r x a b c d e f. (a x -> r) -> (b x -> r) -> (c x -> r) -> (d x -> r) -> (e x -> r) -> (f x -> r) -> Coproduct6 a b c d e f x -> r
coproduct6 a b c d e f y = case y of
  Coproduct (Left r) -> a r
  Coproduct (Right _1) -> case _1 of
    Coproduct (Left r) -> b r
    Coproduct (Right _2) -> case _2 of
      Coproduct (Left r) -> c r
      Coproduct (Right _3) -> case _3 of
        Coproduct (Left r) -> d r
        Coproduct (Right _4) -> case _4 of
          Coproduct (Left r) -> e r
          Coproduct (Right _5) -> case _5 of
            Coproduct (Left r) -> f r
            Coproduct (Right _6) -> absurd (unwrap _6)

coproduct7 :: forall r x a b c d e f g. (a x -> r) -> (b x -> r) -> (c x -> r) -> (d x -> r) -> (e x -> r) -> (f x -> r) -> (g x -> r) -> Coproduct7 a b c d e f g x -> r
coproduct7 a b c d e f g y = case y of
  Coproduct (Left r) -> a r
  Coproduct (Right _1) -> case _1 of
    Coproduct (Left r) -> b r
    Coproduct (Right _2) -> case _2 of
      Coproduct (Left r) -> c r
      Coproduct (Right _3) -> case _3 of
        Coproduct (Left r) -> d r
        Coproduct (Right _4) -> case _4 of
          Coproduct (Left r) -> e r
          Coproduct (Right _5) -> case _5 of
            Coproduct (Left r) -> f r
            Coproduct (Right _6) -> case _6 of
              Coproduct (Left r) -> g r
              Coproduct (Right _7) -> absurd (unwrap _7)

coproduct8 :: forall r x a b c d e f g h. (a x -> r) -> (b x -> r) -> (c x -> r) -> (d x -> r) -> (e x -> r) -> (f x -> r) -> (g x -> r) -> (h x -> r) -> Coproduct8 a b c d e f g h x -> r
coproduct8 a b c d e f g h y = case y of
  Coproduct (Left r) -> a r
  Coproduct (Right _1) -> case _1 of
    Coproduct (Left r) -> b r
    Coproduct (Right _2) -> case _2 of
      Coproduct (Left r) -> c r
      Coproduct (Right _3) -> case _3 of
        Coproduct (Left r) -> d r
        Coproduct (Right _4) -> case _4 of
          Coproduct (Left r) -> e r
          Coproduct (Right _5) -> case _5 of
            Coproduct (Left r) -> f r
            Coproduct (Right _6) -> case _6 of
              Coproduct (Left r) -> g r
              Coproduct (Right _7) -> case _7 of
                Coproduct (Left r) -> h r
                Coproduct (Right _8) -> absurd (unwrap _8)

coproduct9 :: forall r x a b c d e f g h i. (a x -> r) -> (b x -> r) -> (c x -> r) -> (d x -> r) -> (e x -> r) -> (f x -> r) -> (g x -> r) -> (h x -> r) -> (i x -> r) -> Coproduct9 a b c d e f g h i x -> r
coproduct9 a b c d e f g h i y = case y of
  Coproduct (Left r) -> a r
  Coproduct (Right _1) -> case _1 of
    Coproduct (Left r) -> b r
    Coproduct (Right _2) -> case _2 of
      Coproduct (Left r) -> c r
      Coproduct (Right _3) -> case _3 of
        Coproduct (Left r) -> d r
        Coproduct (Right _4) -> case _4 of
          Coproduct (Left r) -> e r
          Coproduct (Right _5) -> case _5 of
            Coproduct (Left r) -> f r
            Coproduct (Right _6) -> case _6 of
              Coproduct (Left r) -> g r
              Coproduct (Right _7) -> case _7 of
                Coproduct (Left r) -> h r
                Coproduct (Right _8) -> case _8 of
                  Coproduct (Left r) -> i r
                  Coproduct (Right _9) -> absurd (unwrap _9)

coproduct10 :: forall r x a b c d e f g h i j. (a x -> r) -> (b x -> r) -> (c x -> r) -> (d x -> r) -> (e x -> r) -> (f x -> r) -> (g x -> r) -> (h x -> r) -> (i x -> r) -> (j x -> r) -> Coproduct10 a b c d e f g h i j x -> r
coproduct10 a b c d e f g h i j y = case y of
  Coproduct (Left r) -> a r
  Coproduct (Right _1) -> case _1 of
    Coproduct (Left r) -> b r
    Coproduct (Right _2) -> case _2 of
      Coproduct (Left r) -> c r
      Coproduct (Right _3) -> case _3 of
        Coproduct (Left r) -> d r
        Coproduct (Right _4) -> case _4 of
          Coproduct (Left r) -> e r
          Coproduct (Right _5) -> case _5 of
            Coproduct (Left r) -> f r
            Coproduct (Right _6) -> case _6 of
              Coproduct (Left r) -> g r
              Coproduct (Right _7) -> case _7 of
                Coproduct (Left r) -> h r
                Coproduct (Right _8) -> case _8 of
                  Coproduct (Left r) -> i r
                  Coproduct (Right _9) -> case _9 of
                    Coproduct (Left r) -> j r
                    Coproduct (Right _10) -> absurd (unwrap _10)
