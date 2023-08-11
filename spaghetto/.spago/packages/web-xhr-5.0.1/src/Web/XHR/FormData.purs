module Web.XHR.FormData
  ( FormData
  , EntryName(..)
  , FileName(..)
  , new
  , fromFormElement
  , append
  , appendBlob
  , delete
  , has
  , set
  , setBlob
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Uncurried as Fn
import Web.File.Blob (Blob)
import Web.HTML.HTMLFormElement (HTMLFormElement)

foreign import data FormData :: Type

newtype EntryName = EntryName String

derive newtype instance eqEntryName :: Eq EntryName
derive newtype instance ordEntryName :: Ord EntryName
derive instance newtypeEntryName :: Newtype EntryName _

newtype FileName = FileName String

derive newtype instance eqFileName :: Eq FileName
derive newtype instance ordFileName :: Ord FileName
derive instance newtypeFileName :: Newtype FileName _

foreign import new :: Effect FormData

fromFormElement :: HTMLFormElement -> Effect FormData
fromFormElement formElement = Fn.runEffectFn1 _fromFormElement formElement

append :: EntryName -> String -> FormData -> Effect Unit
append name value fd = Fn.runEffectFn3 _append name value fd

appendBlob :: EntryName -> Blob -> Maybe FileName -> FormData -> Effect Unit
appendBlob name value filename fd = Fn.runEffectFn4 _appendBlob name value (toNullable filename) fd

delete :: EntryName -> FormData -> Effect Unit
delete name fd = Fn.runEffectFn2 _delete name fd

has :: EntryName -> FormData -> Effect Boolean
has name fd = Fn.runEffectFn2 _has name fd

set :: EntryName -> String -> FormData -> Effect Unit
set name value fd = Fn.runEffectFn3 _set name value fd

setBlob :: EntryName -> Blob -> Maybe FileName -> FormData -> Effect Unit
setBlob name value filename fd = Fn.runEffectFn4 _setBlob name value (toNullable filename) fd

foreign import _fromFormElement :: Fn.EffectFn1 HTMLFormElement FormData

-- void append(USVString name, USVString value);
foreign import _append :: Fn.EffectFn3 EntryName String FormData Unit

-- void append(USVString name, Blob blobValue, optional USVString filename);
foreign import _appendBlob :: Fn.EffectFn4 EntryName Blob (Nullable FileName) FormData Unit

-- void delete(USVString name);
foreign import _delete :: Fn.EffectFn2 EntryName FormData Unit

-- boolean has(USVString name);
foreign import _has :: Fn.EffectFn2 EntryName FormData Boolean

-- void set(USVString name, USVString value);
foreign import _set :: Fn.EffectFn3 EntryName String FormData Unit

-- void set(USVString name, Blob blobValue, optional USVString filename);
foreign import _setBlob :: Fn.EffectFn4 EntryName Blob (Nullable FileName) FormData Unit

-- FormDataEntryValue? get(USVString name);
-- sequence<FormDataEntryValue> getAll(USVString name);
-- iterable<USVString, FormDataEntryValue>;
