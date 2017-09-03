{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Map.Syntax (( ## ))
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import Snap.Core (Method(GET, POST), method, redirect)
import Snap.Snaplet
       (Handler, SnapletInit, addRoutes, makeSnaplet, nestSnaplet, with)
import Snap.Snaplet.Auth
       (AuthManager, addAuthSplices, loginUser, logout, registerUser)
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
       (initPostgresAuth)
import Snap.Snaplet.Heist (heistInit, heistLocal, render)
import Snap.Snaplet.PostgresqlSimple (pgsInit)
import Snap.Snaplet.Session.Backends.CookieSession
       (initCookieSessionManager)
import Snap.Util.FileServe (serveDirectory)

import Accountli.Application
       (App(..), auth, db, heist, migration, sess)
import Accountli.Snaplet.Migration (initMigration)

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

-- | The application initializer.
app :: SnapletInit App App
app =
  makeSnaplet "app" "Accountli App" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db pgsInit
    m <- nestSnaplet "migration" migration $ initMigration d
    s <-
      nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    addAuthSplices h auth
    return $ App h d s a m
