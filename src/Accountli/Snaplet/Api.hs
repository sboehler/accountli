module Accountli.Snaplet.Api where

import Accountli.Snap.Extras (getJSON)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Snap.Core
       (Method(POST), method, modifyResponse, setResponseCode)
import Snap.Snaplet
       (Handler, SnapletInit, SnapletLens, addRoutes, makeSnaplet,
        withTop)
import Snap.Snaplet.Auth (AuthManager, createUser)

data Api =
  Api

data User = User
  { username :: Text
  , password :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON User

postUser :: Handler b (AuthManager b) ()
postUser =
  method POST $ do
    user <- getJSON
    case user of
      Right u -> do
        _ <- createUser (username u) (encodeUtf8 $ password u)
        modifyResponse $ setResponseCode 201
      Left _ -> modifyResponse $ setResponseCode 401

init :: SnapletLens b (AuthManager b) -> SnapletInit b Api
init auth =
  makeSnaplet "api" "Api Snaplet" Nothing $ do
    addRoutes [("", withTop auth postUser)]
    return Api
