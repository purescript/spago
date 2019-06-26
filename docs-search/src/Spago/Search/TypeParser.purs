module Spago.Search.TypeParser where

import Prelude

import Data.Argonaut.Core (Json, caseJsonObject, fromArray, fromObject, jsonEmptyObject, stringify, toArray)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
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

instance encodeJsonQualifiedName :: EncodeJson QualifiedName where
  encodeJson (QualifiedName { moduleName, name }) =
    encodeTuple moduleName name

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

instance encodeJsonKind :: EncodeJson Kind where
  encodeJson = case _ of
    Row k ->
      encodeTaggedContents "Row" (encodeJson k)
    FunKind k1 k2 ->
      encodeTaggedContents "FunKind" (encodeTuple k1 k2)
    NamedKind qname ->
      encodeTaggedContents "NamedKind" (encodeJson qname)

-- | A typeclass constraint
newtype Constraint = Constraint
  { constraintClass :: QualifiedName
  -- ^ constraint class name
  , constraintArgs  :: Array Type
  -- ^ type arguments
  }

derive instance eqConstraint :: Eq Constraint
derive instance genericConstraint :: Generic Constraint _
derive instance newtypeConstraint :: Newtype Constraint _

instance showConstraint :: Show Constraint where
  show = genericShow

instance decodeJsonConstraint :: DecodeJson Constraint where
  decodeJson json = Constraint <$> decodeJson json

instance encodeJsonConstraint :: EncodeJson Constraint where
  encodeJson (Constraint { constraintClass
                         , constraintArgs
                         })
    = fromObject $ Object.fromFoldable
      [ Tuple "constraintClass" (encodeJson constraintClass)
      , Tuple "constraintArgs"  (encodeJson constraintArgs)
      ]

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
  | TypeWildcard
  -- | A type constructor
  | TypeConstructor QualifiedName
  -- | A type operator. This will be desugared into a type constructor during the
  -- "operators" phase of desugaring.
  | TypeOp QualifiedName
  -- | A type application
  | TypeApp Type Type
  -- | Forall quantifier
  | ForAll String (Maybe Kind) Type (Maybe SkolemScope)
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
  | BinaryNoParensType Type Type Type
  -- | Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  | ParensInType Type

type SkolemScope = Unit

derive instance eqType :: Eq Type
derive instance genericType :: Generic Type _

instance showType :: Show Type where
  show = case _ of
    TypeVar _String ->
      "(TypeVar " <> show _String <> ")"
    TypeLevelString _String ->
      "(TypeLevelString " <> show _String <> ")"
    TypeWildcard ->
      "TypeWildcard"
    TypeConstructor _QualifiedName ->
      "(TypeConstructor " <> show _QualifiedName <> ")"
    TypeOp _QualifiedName ->
      "(TypeOp " <> show _QualifiedName <> ")"
    TypeApp _Type1 _Type2 ->
      "(TypeApp " <> show _Type1 <> " " <> show _Type2 <> ")"
    ForAll _String _Maybe_Kind _Type _Maybe_SkolemScope ->
      "(ForAll " <> show _String <> " " <> show _Maybe_Kind <> " " <> show _Type <> " " <> show _Maybe_SkolemScope <> ")"
    ConstrainedType _Constraint _Type ->
      "(ConstrainedType " <> show _Constraint <> " " <> show _Type <> ")"
    REmpty ->
      "REmpty"
    RCons _String _Type1 _Type2 ->
         "(RConsLabel " <> show _String <> " " <> show _Type1 <> " " <> show _Type2 <> ")"
    BinaryNoParensType _Type1 _Type2 _Type3 ->
      "(BinaryNoParensTypeType " <> show _Type1 <> " " <> show _Type2 <> " " <> show _Type3 <> ")"
    ParensInType _Type ->
      "(ParensInType " <> show _Type <> ")"

instance decodeJsonType :: DecodeJson Type where
  decodeJson json = do
    handle <- decodeJson json
    tag <- handle .: "tag"
    case tag of
      "TypeVar" -> handle .: "contents" >>= TypeVar >>> pure
      "TypeLevelString" -> handle .: "contents" >>= TypeLevelString >>> pure
      "TypeConstructor" -> handle .: "contents" >>= TypeConstructor >>> pure
      "TypeOp" -> handle .: "contents" >>= TypeOp >>> pure
      "TypeApp" ->
        decodeContents (decodeTuple TypeApp (const err)) (Left err) json
        where err = mkJsonError' "TypeApp" json
      "ForAll" ->
        decodeContents (decodeTuple (\str ty -> ForAll str Nothing ty Nothing) err) (Left $ err unit) json
        where err = mkJsonError "ForAll" json
      "ConstrainedType" ->
        decodeContents (decodeTuple ConstrainedType err) (Left $ err unit) json
        where err = mkJsonError "ForAll" json
      "REmpty" -> Right REmpty
      "RCons" ->
        decodeContents (decodeTriple RCons (const err)) (Left err) json
        where err = mkJsonError' "RCons" json
      "BinaryNoParensType" ->
        decodeContents (decodeTriple BinaryNoParensType (const err)) (Left err) json
        where err = mkJsonError' "BinaryNoParens" json
      "ParensInType" -> decodeContents
                        (map ParensInType <<< decodeJson)
                        (Left $ mkJsonError' "ParensInType" json)
                        json
      "TypeWildcard" -> Right TypeWildcard
      _ -> Left $ mkJsonError' "Type" json

instance encodeJsonType :: EncodeJson Type where
  encodeJson = case _ of
    TypeVar         val -> encodeTaggedContents "TypeVar"         (encodeJson val)
    TypeLevelString val -> encodeTaggedContents "TypeLevelString" (encodeJson val)
    TypeConstructor val -> encodeTaggedContents "TypeConstructor" (encodeJson val)
    TypeOp          val -> encodeTaggedContents "TypeOp"          (encodeJson val)
    TypeApp t1 t2       -> encodeTaggedContents "TypeApp"         (encodeTuple t1 t2)
    ForAll str _ ty _   -> encodeTaggedContents "ForAll"          (encodeTuple str ty) -- TODO
    ConstrainedType c t -> encodeTaggedContents "ConstrainedType" (encodeTuple c t)
    REmpty              -> encodeTaggedContents "REmpty"          jsonEmptyObject
    RCons s t1 t2       -> encodeTaggedContents "RCons"           (encodeTriple s t1 t2)
    BinaryNoParensType t1 t2 t3 ->
      encodeTaggedContents "BinaryNoParensType" (encodeTriple t1 t2 t3)
    ParensInType t      -> encodeTaggedContents "ParensInType"    (encodeJson t)
    TypeWildcard        -> encodeTaggedContents "TypeWildcard"    jsonEmptyObject

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
  fromMaybe (Left $ err unit) $
  toArray json >>= \jsons ->
  jsons !! 0 >>= \json1 ->
  jsons !! 1 >>= \json2 -> pure $ do
    fst <- decodeJson json1
    sec <- decodeJson json2
    pure $ cont fst sec

-- | Decode a heterogeneous triple.
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
    Just [ json1, json2, json3 ] -> do
      fst <- decodeJson json1
      sec <- decodeJson json2
      trd <- decodeJson json3
      pure $ cont fst sec trd
    _ -> Left $ err unit

-- | Decode a `.contents` property.
decodeContents :: forall r. (Json -> r) -> r -> Json -> r
decodeContents go err json =
  caseJsonObject err
    (\objJson ->
      case Object.lookup "contents" objJson of
        Nothing -> err
        Just contentsJson -> go contentsJson
    )
    json

encodeTuple
  :: forall fst sec
  .  EncodeJson fst
  => EncodeJson sec
  => fst
  -> sec
  -> Json
encodeTuple fst sec =
  fromArray [ encodeJson fst, encodeJson sec ]

encodeTriple
  :: forall fst sec trd
  .  EncodeJson fst
  => EncodeJson sec
  => EncodeJson trd
  => fst
  -> sec
  -> trd
  -> Json
encodeTriple fst sec trd =
  fromArray [ encodeJson fst, encodeJson sec, encodeJson trd ]

encodeTaggedContents :: String -> Json -> Json
encodeTaggedContents tag contents =
  fromObject $ Object.fromFoldable
  [ Tuple "tag" (encodeJson tag)
  , Tuple "contents" contents
  ]
