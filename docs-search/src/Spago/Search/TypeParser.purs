module Spago.Search.TypeParser where

import Control.Alt
import Data.Argonaut.Core
import Data.Either
import Data.Maybe
import Data.Newtype
import Data.Traversable
import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Object as Object

derive instance eqQualifiedName :: Eq QualifiedName
derive instance genericQualifiedName :: Generic QualifiedName _
derive instance newtypeQualifiedName :: Newtype QualifiedName _

instance showQualifiedName :: Show QualifiedName where
  show = genericShow

newtype QualifiedName = QualifiedName { moduleName :: Array String
                                      , name :: String
                                      }

instance decodeJsonQualifiedName :: DecodeJson QualifiedName where
  decodeJson json = do
    decodeTuple
      (\moduleName name -> QualifiedName { moduleName, name })
      (mkJsonError "QualifiedName" json)
      json

mkJsonError :: String -> Json -> (forall i. i -> String)
mkJsonError name json _ =
  "Couldn't parse " <> name <> " from " <> stringify json

mkJsonError' :: String -> Json -> String
mkJsonError' name json = mkJsonError name json unit

-- | The data type of kinds
data Kind
  -- | Kinds for labelled, unordered rows without duplicates
  = Row Kind
  -- | Function kinds
  | FunKind Kind Kind
  -- | A named kind
  | NamedKind QualifiedName

derive instance eqKind :: Eq Kind
derive instance genericKind :: Generic Kind _

instance showKind :: Show Kind where
  show = case _ of
    Row k -> "(Row " <> show k <> ")"
    FunKind k1 k2 -> "(FunKind " <> show k1 <> " " <> show k2 <> ")"
    NamedKind name -> "(NamedKind " <> show name <> ")"

newtype MaybeSingle a = MaybeSingle (Either a (Array a))

derive instance eqMaybeSingle :: Eq a => Eq (MaybeSingle a)
derive instance genericMaybeSingle :: Generic (MaybeSingle a) _
derive instance newtypeMaybeSingle :: Newtype (MaybeSingle a) _

instance showMaybeSingle :: Show a => Show (MaybeSingle a) where
  show = genericShow

instance decodeJsonMaybeSingle :: DecodeJson a => DecodeJson (MaybeSingle a) where
  decodeJson json =
    MaybeSingle <$> (Left <$> decodeJson json <|> Right <$> decodeJson json)

fromMaybeSingle :: forall a. MaybeSingle a -> Array a
fromMaybeSingle (MaybeSingle (Left a)) = Array.singleton a
fromMaybeSingle (MaybeSingle (Right a)) = a

instance decodeJsonKind :: DecodeJson Kind where
  decodeJson json = do
    handle <- decodeJson json
    tag <- handle .: "tag"
    case tag of
      "NamedKind" -> do
        contents <- handle .: "contents"
        pure $ NamedKind contents
      "Row" -> do
        contents <- handle .: "contents"
        pure $ Row contents
      "FunKind" -> do
        contents <- handle .: "contents"
        case contents of
          [k1, k2] ->
            Right $ FunKind k1 k2
          _ -> Left $ mkJsonError' "FunKind" json
      _ -> Left $ mkJsonError' "Kind" json

-- | A typeclass constraint
newtype Constraint = Constraint
  { constraintClass :: QualifiedName
  -- ^ constraint class name
  , constraintArgs  :: Array Unit
  -- ^ type arguments
  }

derive instance eqConstraint :: Eq Constraint
derive instance genericConstraint :: Generic Constraint _
derive instance newtypeConstraint :: Newtype Constraint _

instance showConstraint :: Show Constraint where
  show = genericShow

instance decodeJsonConstraint :: DecodeJson Constraint where
  decodeJson json = Constraint <$> decodeJson json

type ConstraintData = Unit

-- |
-- The type of types
--
data Type
  {-
  -- | A unification variable of type Type
  = TUnknown Int
  -}
  -- | A named type variable
  = TypeVar String
  -- | A type-level string
  | TypeLevelString String
  -- | A type wildcard, as would appear in a partial type synonym
  | TypeWildcard (Maybe String)
  -- | A type constructor
  | TypeConstructor QualifiedName
  -- | A type operator. This will be desugared into a type constructor during the
  -- "operators" phase of desugaring.
  | TypeOp QualifiedName
  -- | A type application
  | TypeApp Type Type
  -- | Forall quantifier
  | ForAll String {- (Maybe Kind) -} Type {- (Maybe SkolemScope) -}
  -- | A type withset of type class constraints
  | ConstrainedType Constraint Type
  {-
  -- | A skolem constant
  | SkolemText Int SkolemScope
  -}
  -- | An empty row
  | REmpty
  -- | A non-empty row
  | RCons String Type Type
  {-
  -- | A type with a kind annotation
  | Kinded Type Kind
  -}
  -- | Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  | BinaryNoParens Type Type Type
  -- | Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  | ParensInType Type

derive instance eqType :: Eq Type
derive instance genericType :: Generic Type _

instance showType :: Show Type where
  show = case _ of
    TypeVar _String ->
      "(TypeVar " <> show _String <> ")"
    TypeLevelString _String ->
      "(TypeLevelString " <> show _String <> ")"
    TypeWildcard _Maybe_String ->
      "(TypeWildcard " <> show _Maybe_String <> ")"
    TypeConstructor _QualifiedName ->
      "(TypeConstructor " <> show _QualifiedName <> ")"
    TypeOp _QualifiedName ->
      "(TypeOp " <> show _QualifiedName <> ")"
    TypeApp _Type1 _Type2 ->
      "(TypeApp " <> show _Type1 <> " " <> show _Type2 <> ")"
    ForAll _String _Type ->
      "(ForAll " <> show _String <> " " <> show _Type <> ")"
    ConstrainedType _Constraint _Type ->
      "(ConstrainedType " <> show _Constraint <> " " <> show _Type <> ")"
    {-
    SkolemText _Int _SkolemScope ->
      "(SkolemText " <> show _Int <> " " <> show _SkolemScope <> ")"
    -}
    REmpty ->
      "REmpty"
    RCons _String _Type1 _Type2 ->
         "(RConsLabel " <> show _String <> " " <> show _Type1 <> " " <> show _Type2 <> ")"
    {-
    Kinded _Type _Kind ->
      "(KindedTypeType " <> show _Type <> " " <> show _Kind <> ")"
    -}
    BinaryNoParens _Type1 _Type2 _Type3 ->
      "(BinaryNoParensTypeType " <> show _Type1 <> " " <> show _Type2 <> " " <> show _Type3 <> ")"
    ParensInType _Type ->
      "(ParensInType " <> show _Type <> ")"

instance decodeJsonType :: DecodeJson Type where
  decodeJson json = do
    handle <- decodeJson json
    tag <- handle .: "tag"
    case tag of
      "TypeVar" -> do
        contents <- handle .: "contents"
        pure $ TypeVar contents
      "TypeLevelString" -> do
        contents <- handle .: "contents"
        pure $ TypeLevelString contents
      "TypeWildCard" -> do
        contents <- handle .:? "contents"
        pure $ TypeWildcard contents
      "TypeConstructor" -> do
        contents <- handle .: "contents"
        pure $ TypeConstructor contents
      "TypeOp" -> do
        contents <- handle .: "contents"
        pure $ TypeOp contents
      "TypeApp" -> do
        contents <- handle .: "contents"
        case contents of
          [t1, t2] -> do
            pure $ TypeApp t1 t2
          _ ->
            Left $ mkJsonError' "TypeApp" json
      "ForAll" ->
        decodeContents
          (decodeTuple ForAll (mkJsonError "ForAll" json))
          (Left $ mkJsonError' "ForAll" json)
          json
      "ConstrainedType" ->
        let err = mkJsonError "ForAll" json in
        decodeContents (decodeTuple ConstrainedType err) (Left $ err unit) json
      "REmpty" ->
        Right REmpty
      "RCons" ->
        let err = mkJsonError' "RCons" json in
          decodeContents
            (decodeTriple
              (\label ty rest ->
                RCons label ty rest)
              (const err))
            (Left $ err)
            json
      "BinaryNoParensType" -> do
        contents <- handle .: "contents"
        case contents of
          [ t1, t2, t3 ] -> do
            pure $ BinaryNoParens t1 t2 t3
          _ -> Left $ mkJsonError' "BinaryNoParens" json
      "ParensInType" ->
        decodeContents
          (map ParensInType <<< decodeJson)
          (Left $ mkJsonError' "ParensInType" json)
          json
      _ -> Left $ mkJsonError' "Type" json


type SkolemScope = Unit

-- | Decode a heterogeneous tuple, serialized as an array.
-- | e.g. `[0, ""]` to `Tuple 0 ""`
decodeTuple
  :: forall fst sec res
  .  DecodeJson fst
  => DecodeJson sec
  => (fst -> sec -> res)
  -> (forall a. a -> String)
  -> Json
  -> Either String res
decodeTuple cont err json =
  case toArray json of
    Nothing -> Left $ err unit
    Just arrOfJsons -> do
      let arrayOfVariants =
            arrOfJsons <#> \variantJson ->
              Left  <$> decodeJson variantJson <|>
              Right <$> decodeJson variantJson
      case arrayOfVariants of
        [ Right (Left a), Right (Right b) ] ->
          Right $ cont a b
        _ -> Left $ err unit

data Triple a b c = T1 a | T2 b | T3 c

decodeTriple
  :: forall fst sec trd res
  .  DecodeJson fst
  => DecodeJson sec
  => DecodeJson trd
  => (fst -> sec -> trd -> res)
  -> (forall a. a -> String)
  -> Json
  -> Either String res
decodeTriple cont err json =
  case toArray json of
    Nothing -> Left $ err unit
    Just arrOfJsons -> do
      let arrayOfVariants =
            arrOfJsons <#> \variantJson ->
              T1 <$> decodeJson variantJson <|>
              T2 <$> decodeJson variantJson <|>
              T3 <$> decodeJson variantJson
      case arrayOfVariants of
        [ Right (T1 a), Right (T2 b), Right (T3 c) ] ->
          Right $ cont a b c
        _ -> Left $ err unit

decodeContents :: forall r. (Json -> r) -> r -> Json -> r
decodeContents go err json =
  caseJsonObject err
    (\objJson ->
      case Object.lookup "contents" objJson of
        Nothing -> err
        Just contentsJson -> go contentsJson
    )
    json
