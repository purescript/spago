module Data.Enum.Generic where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, fromEnum, pred, succ, toEnum)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Bounded.Generic (class GenericBottom, class GenericTop, genericBottom', genericTop')
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)

class GenericEnum a where
  genericPred' :: a -> Maybe a
  genericSucc' :: a -> Maybe a

instance genericEnumNoArguments :: GenericEnum NoArguments where
  genericPred' _ = Nothing
  genericSucc' _ = Nothing

instance genericEnumArgument :: Enum a => GenericEnum (Argument a) where
  genericPred' (Argument a) = Argument <$> pred a
  genericSucc' (Argument a) = Argument <$> succ a

instance genericEnumConstructor :: GenericEnum a => GenericEnum (Constructor name a) where
  genericPred' (Constructor a) = Constructor <$> genericPred' a
  genericSucc' (Constructor a) = Constructor <$> genericSucc' a

instance genericEnumSum :: (GenericEnum a, GenericTop a, GenericEnum b, GenericBottom b) => GenericEnum (Sum a b) where
  genericPred' = case _ of
    Inl a -> Inl <$> genericPred' a
    Inr b -> case genericPred' b of
      Nothing -> Just (Inl genericTop')
      Just b' -> Just (Inr b')
  genericSucc' = case _ of
    Inl a -> case genericSucc' a of
      Nothing -> Just (Inr genericBottom')
      Just a' -> Just (Inl a')
    Inr b -> Inr <$> genericSucc' b

instance genericEnumProduct :: (GenericEnum a, GenericTop a, GenericBottom a, GenericEnum b, GenericTop b, GenericBottom b) => GenericEnum (Product a b) where
  genericPred' (Product a b) = case genericPred' b of
    Just p -> Just $ Product a p
    Nothing -> flip Product genericTop' <$> genericPred' a
  genericSucc' (Product a b) = case genericSucc' b of
    Just s -> Just $ Product a s
    Nothing -> flip Product genericBottom' <$> genericSucc' a


-- | A `Generic` implementation of the `pred` member from the `Enum` type class.
genericPred :: forall a rep. Generic a rep => GenericEnum rep => a -> Maybe a
genericPred = map to <<< genericPred' <<< from

-- | A `Generic` implementation of the `succ` member from the `Enum` type class.
genericSucc :: forall a rep. Generic a rep => GenericEnum rep => a -> Maybe a
genericSucc = map to <<< genericSucc' <<< from

class GenericBoundedEnum a where
  genericCardinality' :: Cardinality a
  genericToEnum' :: Int -> Maybe a
  genericFromEnum' :: a -> Int

instance genericBoundedEnumNoArguments :: GenericBoundedEnum NoArguments where
  genericCardinality' = Cardinality 1
  genericToEnum' i = if i == 0 then Just NoArguments else Nothing
  genericFromEnum' _ = 0

instance genericBoundedEnumArgument :: BoundedEnum a => GenericBoundedEnum (Argument a) where
  genericCardinality' = Cardinality (unwrap (cardinality :: Cardinality a))
  genericToEnum' i = Argument <$> toEnum i
  genericFromEnum' (Argument a) = fromEnum a

instance genericBoundedEnumConstructor :: GenericBoundedEnum a => GenericBoundedEnum (Constructor name a) where
  genericCardinality' = Cardinality (unwrap (genericCardinality' :: Cardinality a))
  genericToEnum' i = Constructor <$> genericToEnum' i
  genericFromEnum' (Constructor a) = genericFromEnum' a

instance genericBoundedEnumSum :: (GenericBoundedEnum a, GenericBoundedEnum b) => GenericBoundedEnum (Sum a b) where
  genericCardinality' =
    Cardinality
      $ unwrap (genericCardinality' :: Cardinality a)
      + unwrap (genericCardinality' :: Cardinality b)
  genericToEnum' n = to genericCardinality'
    where
    to :: Cardinality a -> Maybe (Sum a b)
    to (Cardinality ca)
      | n >= 0 && n < ca = Inl <$> genericToEnum' n
      | otherwise = Inr <$> genericToEnum' (n - ca)
  genericFromEnum' = case _ of
    Inl a -> genericFromEnum' a
    Inr b -> genericFromEnum' b + unwrap (genericCardinality' :: Cardinality a)


instance genericBoundedEnumProduct :: (GenericBoundedEnum a, GenericBoundedEnum b) => GenericBoundedEnum (Product a b) where
  genericCardinality' =
    Cardinality
      $ unwrap (genericCardinality' :: Cardinality a)
      * unwrap (genericCardinality' :: Cardinality b)
  genericToEnum' n = to genericCardinality'
    where to :: Cardinality b -> Maybe (Product a b)
          to (Cardinality cb) = Product <$> (genericToEnum' $ n `div` cb) <*> (genericToEnum' $ n `mod` cb)
  genericFromEnum' = from genericCardinality'
    where from :: Cardinality b -> (Product a b) -> Int
          from (Cardinality cb) (Product a b) = genericFromEnum' a * cb + genericFromEnum' b


-- | A `Generic` implementation of the `cardinality` member from the
-- | `BoundedEnum` type class.
genericCardinality :: forall a rep. Generic a rep => GenericBoundedEnum rep => Cardinality a
genericCardinality = Cardinality (unwrap (genericCardinality' :: Cardinality rep))

-- | A `Generic` implementation of the `toEnum` member from the `BoundedEnum`
-- | type class.
genericToEnum :: forall a rep. Generic a rep => GenericBoundedEnum rep => Int -> Maybe a
genericToEnum = map to <<< genericToEnum'

-- | A `Generic` implementation of the `fromEnum` member from the `BoundedEnum`
-- | type class.
genericFromEnum :: forall a rep. Generic a rep => GenericBoundedEnum rep => a -> Int
genericFromEnum = genericFromEnum' <<< from
