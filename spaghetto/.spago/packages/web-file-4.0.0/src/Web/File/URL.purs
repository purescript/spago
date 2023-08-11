module Web.File.Url 
  (createObjectURL
  , revokeObjectURL
  ) where

import Prelude
import Effect (Effect)
import Web.File.Blob (Blob)


-- | Adds this blob to the url store
-- | The string is a url that can be used to
-- | 'download' the blob
foreign import createObjectURL :: Blob -> Effect String

-- | Revoke a blob url from the url store
-- | Doesn't throw errors on failure
foreign import revokeObjectURL :: String -> Effect Unit