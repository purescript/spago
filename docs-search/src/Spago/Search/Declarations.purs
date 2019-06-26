module Spago.Search.Declarations where

import Prelude

import Spago.Search.TypeParser

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json, fromString, stringify, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (Aff)

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
    DeclValue -> "value"
    DeclData -> "data"
    DeclExternData -> "externData"
    DeclTypeSynonym -> "typeSynonym"
    DeclTypeClass -> "typeClass"
    DeclAlias -> "alias"
    DeclExternKind -> "kind"

instance decodeJsonDeclType :: DecodeJson DeclType where
  decodeJson json =
    case toString json of
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
      Nothing           -> Left $ "Couldn't decode DeclType: " <> stringify json

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
      Just string ->
        case string of
          "instance" -> Right ChildDeclInstance
          "dataConstructor" -> Right ChildDeclDataConstructor
          "typeClassMember" -> Right ChildDeclTypeClassMember
          _             -> Left $ "Couldn't decode ChildDeclType: " <> string
      Nothing           -> Left $ "Couldn't decode ChildDeclType: " <> stringify json

newtype IndexEntry
  = IndexEntry { title :: String
               , comments :: Maybe String
               , info :: { declType :: DeclType
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
               , children :: Array ChildIndexEntry
               }

derive instance eqIndexEntry :: Eq IndexEntry
derive instance genericIndexEntry :: Generic IndexEntry _
derive instance newtypeIndexEntry :: Newtype IndexEntry _

instance showIndexEntry :: Show IndexEntry where
  show = genericShow

instance decodeJsonIndexEntry :: DecodeJson IndexEntry where
  decodeJson json = IndexEntry <$> do
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
      pure { type: ty, kind, declType, typeArguments, superclasses, fundeps, arguments }
    sourceSpan <- handle .:  "sourceSpan"
    pure { title, comments, info, sourceSpan, children }

instance encodeJsonIndexEntry :: EncodeJson IndexEntry where
  encodeJson = encodeJson <<< unwrap

newtype ChildIndexEntry
  = ChildIndexEntry { title :: String
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

derive instance eqChildIndexEntry :: Eq ChildIndexEntry
derive instance genericChildIndexEntry :: Generic ChildIndexEntry _
derive instance newtypeChildIndexEntry :: Newtype ChildIndexEntry _
instance showChildIndexEntry :: Show ChildIndexEntry where
  show = genericShow

instance decodeJsonChildIndexEntry :: DecodeJson ChildIndexEntry where
  decodeJson json = ChildIndexEntry <$> do
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

instance encodeJsonChildIndexEntry :: EncodeJson ChildIndexEntry where
  encodeJson = encodeJson <<< unwrap

newtype Declarations
  = Declarations { name :: String
                 , declarations :: Array IndexEntry
                 }

derive instance eqDeclarations :: Eq Declarations
derive instance genericDeclarations :: Generic Declarations _
derive instance newtypeDeclarations :: Newtype Declarations _

instance showDeclarations :: Show Declarations where
  show = genericShow

instance decodeJsonDeclarations :: DecodeJson Declarations where
  decodeJson json = Declarations <$> decodeJson json

instance encodeJsonDeclarations :: EncodeJson Declarations where
  encodeJson = encodeJson <<< unwrap

loadDeclarations :: String -> Aff (Either String (Array Declarations))
loadDeclarations string = decodeJson <$> toAffE (loadDeclarations_ string)

foreign import loadDeclarations_ :: String -> Effect (Promise Json)
