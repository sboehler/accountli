module Accountli.Snaplet.Migration where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Database.PostgreSQL.Simple.Migration
       (MigrationCommand(..), MigrationContext(..), runMigration)
import Snap.Snaplet
       (Snaplet, SnapletInit, getSnapletFilePath, makeSnaplet)
import Snap.Snaplet.PostgresqlSimple (Postgres, liftPG')

data Migration =
  Migration

migrate :: FilePath -> Connection -> IO ()
migrate path con =
  withTransaction con $
  runMigration (MigrationContext MigrationInitialization True con) >>
  runMigration (MigrationContext (MigrationDirectory path) True con) >>
  return ()

initMigration :: Snaplet Postgres -> SnapletInit b Migration
initMigration db =
  makeSnaplet "migrations" "Migration Snaplet" Nothing $ do
    path <- getSnapletFilePath
    _ <- liftIO $ runReaderT (liftPG' $ migrate path) db
    return Migration
