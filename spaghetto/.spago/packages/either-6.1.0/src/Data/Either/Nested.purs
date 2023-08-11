-- | Utilities for n-eithers: sums types with more than two terms built from nested eithers.
-- |
-- | Nested eithers arise naturally in sum combinators. You shouldn't
-- | represent sum data using nested eithers, but if combinators you're working with
-- | create them, utilities in this module will allow to to more easily work
-- | with them, including translating to and from more traditional sum types.
-- |
-- | ```purescript
-- | data Color = Red Number | Green Number | Blue Number
-- |
-- | fromEither3 :: Either3 Number Number Number -> Color
-- | fromEither3 = either3 Red Green Blue
-- |
-- | toEither3 :: Color -> Either3 Number Number Number
-- | toEither3 (Red   v) = in1 v
-- | toEither3 (Green v) = in2 v
-- | toEither3 (Blue  v) = in3 v
-- | ```
module Data.Either.Nested
  ( type (\/), (\/)
  , in1, in2, in3, in4, in5, in6, in7, in8, in9, in10
  , at1, at2, at3, at4, at5, at6, at7, at8, at9, at10
  , Either1, Either2, Either3, Either4, Either5, Either6, Either7, Either8, Either9, Either10
  , either1, either2, either3, either4, either5, either6, either7, either8, either9, either10
  ) where

import Data.Either (Either(..), either)
import Data.Void (Void, absurd)

infixr 6 type Either as \/

-- | The `\/` operator alias for the `either` function allows easy matching on nested Eithers. For example, consider the function
-- |
-- | ```purescript
-- | f :: (Int \/ String \/ Boolean) -> String
-- | f (Left x) = show x
-- | f (Right (Left y)) = y
-- | f (Right (Right z)) = if z then "Yes" else "No"
-- | ```
-- |
-- | The `\/` operator alias allows us to rewrite this function as
-- |
-- | ```purescript
-- | f :: (Int \/ String \/ Boolean) -> String
-- | f = show \/ identity \/ if _ then "Yes" else "No"
-- | ```
infixr 6 either as \/

type Either1 a = a \/ Void
type Either2 a b = a \/ b \/ Void
type Either3 a b c = a \/ b \/ c \/ Void
type Either4 a b c d = a \/ b \/ c \/ d \/ Void
type Either5 a b c d e = a \/ b \/ c \/ d \/ e \/ Void
type Either6 a b c d e f = a \/ b \/ c \/ d \/ e \/ f \/ Void
type Either7 a b c d e f g = a \/ b \/ c \/ d \/ e \/ f \/ g \/ Void
type Either8 a b c d e f g h = a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ Void
type Either9 a b c d e f g h i = a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ Void
type Either10 a b c d e f g h i j = a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ j \/ Void

in1 :: forall a z. a -> a \/ z
in1 = Left

in2 :: forall a b z. b -> a \/ b \/ z
in2 v = Right (Left v)

in3 :: forall a b c z. c -> a \/ b \/ c \/ z
in3 v = Right (Right (Left v))

in4 :: forall a b c d z. d -> a \/ b \/ c \/ d \/ z
in4 v = Right (Right (Right (Left v)))

in5 :: forall a b c d e z. e -> a \/ b \/ c \/ d \/ e \/ z
in5 v = Right (Right (Right (Right (Left v))))

in6 :: forall a b c d e f z. f -> a \/ b \/ c \/ d \/ e \/ f \/ z
in6 v = Right (Right (Right (Right (Right (Left v)))))

in7 :: forall a b c d e f g z. g -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ z
in7 v = Right (Right (Right (Right (Right (Right (Left v))))))

in8 :: forall a b c d e f g h z. h -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ z
in8 v = Right (Right (Right (Right (Right (Right (Right (Left v)))))))

in9 :: forall a b c d e f g h i z. i -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ z
in9 v = Right (Right (Right (Right (Right (Right (Right (Right (Left v))))))))

in10 :: forall a b c d e f g h i j z. j -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ j \/ z
in10 v = Right (Right (Right (Right (Right (Right (Right (Right (Right (Left v)))))))))

at1 :: forall r a z. r -> (a -> r) -> a \/ z -> r
at1 b f y = case y of
  Left r -> f r
  _ -> b

at2 :: forall r a b z. r -> (b -> r) -> a \/ b \/ z -> r
at2 b f y = case y of
  Right (Left r) -> f r
  _ -> b

at3 :: forall r a b c z. r -> (c -> r) -> a \/ b \/ c \/ z -> r
at3 b f y = case y of
  Right (Right (Left r)) -> f r
  _ -> b

at4 :: forall r a b c d z. r -> (d -> r) -> a \/ b \/ c \/ d \/ z -> r
at4 b f y = case y of
  Right (Right (Right (Left r))) -> f r
  _ -> b

at5 :: forall r a b c d e z. r -> (e -> r) -> a \/ b \/ c \/ d \/ e \/ z -> r
at5 b f y = case y of
  Right (Right (Right (Right (Left r)))) -> f r
  _ -> b

at6 :: forall r a b c d e f z. r -> (f -> r) -> a \/ b \/ c \/ d \/ e \/ f \/ z -> r
at6 b f y = case y of
  Right (Right (Right (Right (Right (Left r))))) -> f r
  _ -> b

at7 :: forall r a b c d e f g z. r -> (g -> r) -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ z -> r
at7 b f y = case y of
  Right (Right (Right (Right (Right (Right (Left r)))))) -> f r
  _ -> b

at8 :: forall r a b c d e f g h z. r -> (h -> r) -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ z -> r
at8 b f y = case y of
  Right (Right (Right (Right (Right (Right (Right (Left r))))))) -> f r
  _ -> b

at9 :: forall r a b c d e f g h i z. r -> (i -> r) -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ z -> r
at9 b f y = case y of
  Right (Right (Right (Right (Right (Right (Right (Right (Left r)))))))) -> f r
  _ -> b

at10 :: forall r a b c d e f g h i j z. r -> (j -> r) -> a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ j \/ z -> r
at10 b f y = case y of
  Right (Right (Right (Right (Right (Right (Right (Right (Right (Left r))))))))) -> f r
  _ -> b

either1 :: forall a. Either1 a -> a
either1 y = case y of
  Left r -> r
  Right _1 -> absurd _1

either2 :: forall r a b. (a -> r) -> (b -> r) -> Either2 a b -> r
either2 a b y = case y of
  Left r -> a r
  Right _1 -> case _1 of
    Left r -> b r
    Right _2 -> absurd _2

either3 :: forall r a b c. (a -> r) -> (b -> r) -> (c -> r) -> Either3 a b c -> r
either3 a b c y = case y of
  Left r -> a r
  Right _1 -> case _1 of
    Left r -> b r
    Right _2 -> case _2 of
      Left r -> c r
      Right _3 -> absurd _3

either4 :: forall r a b c d. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> Either4 a b c d -> r
either4 a b c d y = case y of
  Left r -> a r
  Right _1 -> case _1 of
    Left r -> b r
    Right _2 -> case _2 of
      Left r -> c r
      Right _3 -> case _3 of
        Left r -> d r
        Right _4 -> absurd _4

either5 :: forall r a b c d e. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> Either5 a b c d e -> r
either5 a b c d e y = case y of
  Left r -> a r
  Right _1 -> case _1 of
    Left r -> b r
    Right _2 -> case _2 of
      Left r -> c r
      Right _3 -> case _3 of
        Left r -> d r
        Right _4 -> case _4 of
          Left r -> e r
          Right _5 -> absurd _5

either6 :: forall r a b c d e f. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> Either6 a b c d e f -> r
either6 a b c d e f y = case y of
  Left r -> a r
  Right _1 -> case _1 of
    Left r -> b r
    Right _2 -> case _2 of
      Left r -> c r
      Right _3 -> case _3 of
        Left r -> d r
        Right _4 -> case _4 of
          Left r -> e r
          Right _5 -> case _5 of
            Left r -> f r
            Right _6 -> absurd _6

either7 :: forall r a b c d e f g. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> Either7 a b c d e f g -> r
either7 a b c d e f g y = case y of
  Left r -> a r
  Right _1 -> case _1 of
    Left r -> b r
    Right _2 -> case _2 of
      Left r -> c r
      Right _3 -> case _3 of
        Left r -> d r
        Right _4 -> case _4 of
          Left r -> e r
          Right _5 -> case _5 of
            Left r -> f r
            Right _6 -> case _6 of
              Left r -> g r
              Right _7 -> absurd _7

either8 :: forall r a b c d e f g h. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> (h -> r) -> Either8 a b c d e f g h -> r
either8 a b c d e f g h y = case y of
  Left r -> a r
  Right _1 -> case _1 of
    Left r -> b r
    Right _2 -> case _2 of
      Left r -> c r
      Right _3 -> case _3 of
        Left r -> d r
        Right _4 -> case _4 of
          Left r -> e r
          Right _5 -> case _5 of
            Left r -> f r
            Right _6 -> case _6 of
              Left r -> g r
              Right _7 -> case _7 of
                Left r -> h r
                Right _8 -> absurd _8

either9 :: forall r a b c d e f g h i. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> (h -> r) -> (i -> r) -> Either9 a b c d e f g h i -> r
either9 a b c d e f g h i y = case y of
  Left r -> a r
  Right _1 -> case _1 of
    Left r -> b r
    Right _2 -> case _2 of
      Left r -> c r
      Right _3 -> case _3 of
        Left r -> d r
        Right _4 -> case _4 of
          Left r -> e r
          Right _5 -> case _5 of
            Left r -> f r
            Right _6 -> case _6 of
              Left r -> g r
              Right _7 -> case _7 of
                Left r -> h r
                Right _8 -> case _8 of
                  Left r -> i r
                  Right _9 -> absurd _9

either10 :: forall r a b c d e f g h i j. (a -> r) -> (b -> r) -> (c -> r) -> (d -> r) -> (e -> r) -> (f -> r) -> (g -> r) -> (h -> r) -> (i -> r) -> (j -> r) -> Either10 a b c d e f g h i j -> r
either10 a b c d e f g h i j y = case y of
  Left r -> a r
  Right _1 -> case _1 of
    Left r -> b r
    Right _2 -> case _2 of
      Left r -> c r
      Right _3 -> case _3 of
        Left r -> d r
        Right _4 -> case _4 of
          Left r -> e r
          Right _5 -> case _5 of
            Left r -> f r
            Right _6 -> case _6 of
              Left r -> g r
              Right _7 -> case _7 of
                Left r -> h r
                Right _8 -> case _8 of
                  Left r -> i r
                  Right _9 -> case _9 of
                    Left r -> j r
                    Right _10 -> absurd _10
