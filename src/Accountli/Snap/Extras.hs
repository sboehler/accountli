module Accountli.Snap.Extras where

import Data.Aeson
       (FromJSON, Result(Error, Success), decode, fromJSON)
import Data.Int (Int64)
import Snap (MonadSnap, readRequestBody)

-------------------------------------------------------------------------------
-- | Try to parse request body as JSON with a default max size of
-- 50000.
getJSON :: (MonadSnap m, FromJSON a) => m (Either String a)
getJSON = getBoundedJSON 50000

-------------------------------------------------------------------------------
-- | Parse request body into JSON or return an error string.
getBoundedJSON ::
     (MonadSnap m, FromJSON a)
  => Int64
    -- ^ Maximum size in bytes
  -> m (Either String a)
getBoundedJSON n = do
  bodyVal <- decode `fmap` readRequestBody (fromIntegral n)
  return $
    case bodyVal of
      Nothing -> Left "Can't find JSON data in POST body"
      Just v ->
        case fromJSON v of
          Error e -> Left e
          Success a -> Right a
