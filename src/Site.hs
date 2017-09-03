{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import Data.Map.Syntax (( ## ))
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Database.PostgreSQL.Simple.Migration
       (MigrationCommand(..), MigrationContext(..), runMigration)
import qualified Heist.Interpreted as I
import Snap.Core (Method(GET, POST), method, redirect)
import Snap.Snaplet
       (Handler, SnapletInit, addRoutes, makeSnaplet, nestSnaplet, with)
import Snap.Snaplet.Auth
       (AuthManager, addAuthSplices, loginUser, logout, registerUser)
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
       (initPostgresAuth)
import Snap.Snaplet.Heist (heistInit, heistLocal, render)
import Snap.Snaplet.PostgresqlSimple (liftPG', pgsInit)
import Snap.Snaplet.Session.Backends.CookieSession
       (initCookieSessionManager)
import Snap.Util.FileServe (serveDirectory)

import Application

-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err

-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = loginUser "login" "password" Nothing (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"

-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"

-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes =
  [ ("login", with auth handleLoginSubmit)
  , ("logout", with auth handleLogout)
  , ("new_user", with auth handleNewUser)
  , ("", serveDirectory "static")
  ]

migrate :: Connection -> IO ()
migrate con =
  let dir = "src/migrations"
  in withTransaction con $
     runMigration (MigrationContext MigrationInitialization True con) >>
     runMigration (MigrationContext (MigrationDirectory dir) True con) >>
     return ()

-- | The application initializer.
app :: SnapletInit App App
app =
  makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db pgsInit
    s <-
      nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    _ <- liftIO $ runReaderT (liftPG' migrate) d
    addRoutes routes
    addAuthSplices h auth
    return $ App h d s a
