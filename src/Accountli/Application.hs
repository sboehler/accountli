module Accountli.Application where

import Control.Lens (makeLenses, set)
import Control.Monad.Reader (local)
import Control.Monad.State (get)
import Snap.Snaplet
       (Handler, Snaplet, snapletValue, subSnaplet, with)
import Snap.Snaplet.Auth (AuthManager)
import Snap.Snaplet.Heist (HasHeist(heistLens), Heist)
import Snap.Snaplet.PostgresqlSimple
       (HasPostgres(getPostgresState, setLocalPostgresState), Postgres)
import Snap.Snaplet.Session (SessionManager)

data App = App
  { _heist :: Snaplet (Heist App)
  , _db :: Snaplet Postgres
  , _sess :: Snaplet SessionManager
  , _auth :: Snaplet (AuthManager App)
  }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
  getPostgresState = with db get
  setLocalPostgresState s = local (set (db . snapletValue) s)

type AppHandler = Handler App App
