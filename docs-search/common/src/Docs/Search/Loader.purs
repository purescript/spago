module Docs.Search.Loader where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Error.Class (throwError)
import Control.Promise (Promise, toAffE)
import Data.Codec.JSON.Common as CJ
import Data.Either (either)
import Data.Newtype (unwrap)
import Docs.Search.Types (GlobalIdentifier, URL)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (error)
import JSON (JSON)

load
  :: forall a
   . CJ.Codec a
  -> GlobalIdentifier
  -> URL
  -> Aff a
load codec globalIdentifier url = do
  json <- toAffE (loadFromScript globalIdentifier url)
  either throw pure $ CJ.decode codec json
  where
  throw err = throwError $ error $
    "Couldn't load content from window."
      <> unwrap globalIdentifier
      <> ": "
      <> CJ.DecodeError.print err

foreign import loadFromScript
  :: GlobalIdentifier
  -> URL
  -> Effect (Promise JSON)
