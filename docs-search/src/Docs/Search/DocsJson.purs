-- | A module containing everything that is necessary to decode `docs.json` files.
module Docs.Search.DocsJson where

import Prelude

import Docs.Search.TypeDecoder (Constraint, FunDeps, Kind, Type, TypeArgument)

import Data.Argonaut.Core (fromString, stringify, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)

newtype DocsJson
  = DocsJson { name :: String
             , declarations :: Array Declaration
             }

derive instance eqDocsJson :: Eq DocsJson
derive instance genericDocsJson :: Generic DocsJson _
derive instance newtypeDocsJson :: Newtype DocsJson _

instance showDocsJson :: Show DocsJson where
  show = genericShow

instance decodeJsonDocsJson :: DecodeJson DocsJson where
  decodeJson json = DocsJson <$> decodeJson json

instance encodeJsonDocsJson :: EncodeJson DocsJson where
  encodeJson = encodeJson <<< unwrap

newtype Declaration
  = Declaration { title :: String
               , comments :: Maybe String
               , info :: { declType      :: DeclType
                         , dataDeclType  :: Maybe DataDeclType
                         , kind          :: Maybe Kind
                         , typeArguments :: Maybe (Array TypeArgument)
                         , type          :: Maybe Type
                         , superclasses  :: Maybe (Array Constraint)
                         , arguments     :: Maybe (Array TypeArgument)
                         , fundeps       :: Maybe FunDeps
                         }
               , sourceSpan :: { start :: Array Int
                               , end :: Array Int
                               , name :: String
                               }
               , children :: Array ChildDeclaration
               }

derive instance eqDeclaration :: Eq Declaration
derive instance genericDeclaration :: Generic Declaration _
derive instance newtypeDeclaration :: Newtype Declaration _

instance showDeclaration :: Show Declaration where
  show = genericShow

instance decodeJsonDeclaration :: DecodeJson Declaration where
  decodeJson json = Declaration <$> do
    handle     <- decodeJson json
    title      <- handle .:  "title"
    comments   <- handle .:? "comments"
    children   <- handle .:  "children"
    info       <- handle .:  "info" >>= \info -> do
      ty            <- info .:? "type"
      kind          <- info .:? "kind"
      typeArguments <- info .:? "typeArguments"
      arguments     <- info .:? "arguments"
      superclasses  <- info .:? "superclasses"
      fundeps       <- info .:? "fundeps"
      declType      <- info .:  "declType"
      dataDeclType  <- info .:? "dataDeclType"
      pure { type: ty, kind, declType, typeArguments, superclasses, fundeps
           , arguments, dataDeclType }
    sourceSpan <- handle .:  "sourceSpan"
    pure { title, comments, info, sourceSpan, children }

instance encodeJsonDeclaration :: EncodeJson Declaration where
  encodeJson = encodeJson <<< unwrap

newtype ChildDeclaration
  = ChildDeclaration { title :: String
                     , comments :: Maybe String
                     , info :: { declType :: ChildDeclType
                               , arguments :: Maybe (Array Type)
                               , type :: Maybe Type
                               }
                     , mbSourceSpan :: Maybe { start :: Array Int
                                             , end :: Array Int
                                             , name :: String
                                             }
                     }

derive instance eqChildDeclaration :: Eq ChildDeclaration
derive instance genericChildDeclaration :: Generic ChildDeclaration _
derive instance newtypeChildDeclaration :: Newtype ChildDeclaration _

instance showChildDeclaration :: Show ChildDeclaration where
  show = genericShow

instance decodeJsonChildDeclaration :: DecodeJson ChildDeclaration where
  decodeJson json = ChildDeclaration <$> do
    handle       <- decodeJson json
    title        <- handle .:  "title"
    comments     <- handle .:? "comments"
    info         <- handle .:  "info" >>= \info -> do
      arguments <- info .:? "arguments"
      ty        <- info .:? "type"
      declType  <- info .:  "declType"
      pure { arguments, declType, type: ty }
    mbSourceSpan <- handle .:?  "sourceSpan"
    pure { title, comments, info, mbSourceSpan }

instance encodeJsonChildDeclaration :: EncodeJson ChildDeclaration where
  encodeJson = encodeJson <<< unwrap

-- See `src/Language/Purescript/Docs/Types.hs`
data DeclType
  = DeclValue
  | DeclData
  | DeclExternData
  | DeclTypeSynonym
  | DeclTypeClass
  | DeclAlias
  | DeclExternKind

derive instance eqDeclType :: Eq DeclType
derive instance genericDeclType :: Generic DeclType _

instance showDeclType :: Show DeclType where
  show = genericShow

instance encodeJsonDeclType :: EncodeJson DeclType where
  encodeJson = fromString <<< case _ of
    DeclValue       -> "value"
    DeclData        -> "data"
    DeclExternData  -> "externData"
    DeclTypeSynonym -> "typeSynonym"
    DeclTypeClass   -> "typeClass"
    DeclAlias       -> "alias"
    DeclExternKind  -> "kind"

instance decodeJsonDeclType :: DecodeJson DeclType where
  decodeJson json =
    case toString json of
      Nothing     -> Left $ "Couldn't decode DeclType: " <> stringify json
      Just string ->
        case string of
          "value"       -> Right DeclValue
          "data"        -> Right DeclData
          "externData"  -> Right DeclExternData
          "typeSynonym" -> Right DeclTypeSynonym
          "typeClass"   -> Right DeclTypeClass
          "alias"       -> Right DeclAlias
          "kind"        -> Right DeclExternKind
          _             -> Left $ "Couldn't decode DeclType: " <> string

data ChildDeclType
  = ChildDeclInstance
  | ChildDeclDataConstructor
  | ChildDeclTypeClassMember

derive instance eqChildDeclType :: Eq ChildDeclType
derive instance genericChildDeclType :: Generic ChildDeclType _

instance showChildDeclType :: Show ChildDeclType where
  show = genericShow

instance encodeJsonChildDeclType :: EncodeJson ChildDeclType where
  encodeJson = fromString <<< case _ of
    ChildDeclInstance        -> "instance"
    ChildDeclDataConstructor -> "dataConstructor"
    ChildDeclTypeClassMember -> "typeClassMember"

instance decodeJsonChildDeclType :: DecodeJson ChildDeclType where
  decodeJson json =
    case toString json of
      Nothing     -> Left $ "Couldn't decode ChildDeclType: " <> stringify json
      Just tag ->
        case tag of
          "instance" -> Right ChildDeclInstance
          "dataConstructor" -> Right ChildDeclDataConstructor
          "typeClassMember" -> Right ChildDeclTypeClassMember
          _                 -> Left $ "Couldn't decode ChildDeclType: " <> tag

data DataDeclType
  = NewtypeDataDecl
  | DataDataDecl

derive instance eqDataDeclType :: Eq DataDeclType
derive instance genericDataDeclType :: Generic DataDeclType _

instance showDataDeclType :: Show DataDeclType where
  show = genericShow

instance encodeJsonDataDeclType :: EncodeJson DataDeclType where
  encodeJson = fromString <<< case _ of
    NewtypeDataDecl -> "newtype"
    DataDataDecl    -> "data"

instance decodeJsonDataDeclType :: DecodeJson DataDeclType where
  decodeJson json =
    case toString json of
      Just tag ->
        case tag of
          "newtype" -> Right NewtypeDataDecl
          "data"    -> Right DataDataDecl
          _         -> Left $ "Couldn't decode DataDeclType: " <> tag
      Nothing     -> Left $ "Couldn't decode DataDeclType: "   <> stringify json
