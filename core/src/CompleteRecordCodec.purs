-- | A reimplementation of functions from Data.Codec.JSON that have to do with
-- | records, but producing an error upon encountering an unrecognized field in
-- | the JSON object.
module Spago.Core.CompleteRecordCodec where

import Spago.Core.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Except (lift, throwError, withExceptT)
import Control.Monad.State (modify_, runStateT)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.List as List
import Data.Set as Set
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import JSON (JObject, JSON)
import JSON.Object as JO
import JSON.Path as JP
import Prim.Row as Row
import Record.Unsafe as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type PropCodec a =
  Codec.Codec
    (StateT { claimedProps :: Set String } (Except CJ.DecodeError.DecodeError))
    JObject
    (List.List (String /\ JSON))
    a
    a

object ∷ ∀ a. PropCodec a -> CJ.Codec a
object codec = Codec.codec' dec enc
  where
  dec j = do
    obj <- Codec.decode CJ.jobject j
    rec /\ { claimedProps } <- runStateT (Codec.decode codec obj) { claimedProps: Set.empty }

    let unclaimedProps = Set.difference (Set.fromFoldable (JO.keys obj)) claimedProps
    when (not Set.isEmpty unclaimedProps) do
      throwError $ CJ.DecodeError.error JP.Tip $ "Unknown field(s): " <> joinWith ", " (Set.toUnfoldable unclaimedProps)

    pure rec

  enc a = Codec.encode CJ.jobject $ JO.fromFoldable $ Codec.encode codec a

record ∷ PropCodec {}
record = Codec.Codec (const (pure {})) pure

recordProp
  :: ∀ @p a r r'
   . IsSymbol p
  => Row.Cons p a r r'
  => CJ.Codec a
  -> PropCodec (Record r)
  -> PropCodec (Record r')
recordProp codecA codecR = Codec.codec dec enc
  where
  key = reflectSymbol $ Proxy @p
  liftError = CJ.DecodeError.withPath (JP.AtKey key)

  dec ∷ JObject -> StateT _ (Except CJ.DecodeError.DecodeError) (Record r')
  dec obj = do
    r <- Codec.decode codecR obj
    a :: a <- case JO.lookup key obj of
      Just val -> lift $ withExceptT liftError $ Codec.decode codecA val
      Nothing -> throwError $ CJ.DecodeError.noValueFound (JP.AtKey key JP.Tip)
    modify_ \s -> { claimedProps: Set.insert key s.claimedProps }
    pure $ Record.unsafeSet key a r

  enc ∷ Record r' -> List.List (Tuple String JSON)
  enc val =
    Tuple key (Codec.encode codecA (Record.unsafeGet key val))
      : Codec.encode codecR ((unsafeCoerce ∷ Record r' -> Record r) val)

recordPropOptional
  :: ∀ @p a r r'
   . IsSymbol p
  => Row.Cons p (Maybe a) r r'
  => CJ.Codec a
  -> PropCodec (Record r)
  -> PropCodec (Record r')
recordPropOptional codecA codecR = Codec.codec dec enc
  where
  key = reflectSymbol $ Proxy @p
  liftError = CJ.DecodeError.withPath (JP.AtKey key)

  dec ∷ JObject -> StateT _ (Except CJ.DecodeError.DecodeError) (Record r')
  dec obj = do
    r <- Codec.decode codecR obj
    a :: Maybe a <- case JO.lookup key obj of
      Just val -> lift $ withExceptT liftError $ Just <$> Codec.decode codecA val
      Nothing -> pure Nothing
    modify_ \s -> { claimedProps: Set.insert key s.claimedProps }
    pure $ Record.unsafeSet key a r

  enc ∷ Record r' -> List.List (Tuple String JSON)
  enc val = do
    let r = Codec.encode codecR ((unsafeCoerce ∷ Record r' -> Record r) val)
    case Record.unsafeGet key val of
      Nothing -> r
      Just val' -> Tuple key (Codec.encode codecA val') : r
