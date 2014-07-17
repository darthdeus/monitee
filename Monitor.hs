module Monitor
       ( runMonitor
       , withDB
       , db
       ) where

import Control.Concurrent
import Control.Exception
import qualified Data.Text as T
import Data.String
import Data.Time
import Database.Persist.Postgresql
import Import
import Network.Wreq as Wreq

runMonitor :: App -> IO ()
runMonitor app = do
    let pool = connPool app

    let loop = do
        flip runSqlPersistMPool pool $ do
            processes <- selectList [] [Desc ProcessName]
            mapM_ checkAvailability processes
            return ()

        threadDelay 5000000
        loop

    void loop

checkAvailability :: Entity Process -> SqlPersistM ()
checkAvailability (Entity pid p) = do
    status <- liftIO $ pingServer p
    time <- liftIO $ getCurrentTime

    void $ insert $ Report time pid status
    return ()

tryIO :: IO a -> IO (Either SomeException a)
tryIO = try

pingServer :: Process -> IO Bool
pingServer process = do
    result <- tryIO $ Wreq.get $ process ^. processUrl.to T.unpack

    case result of
      Left _ -> return False
      Right _ -> return True


withDB :: App -> SqlPersistM a -> IO a
withDB app action = do
    let connStr = pgConnStr $ persistConfig app
    withPostgresqlPool connStr 1 (runSqlPersistMPool action)

connString :: Data.String.IsString a => a
connString = "host=localhost dbname=monitee_development user=darth password= port=5432"

-- Run a DB action in the development settings
db :: SqlPersistM a -> IO a
db action = withPostgresqlPool connString 2 (runSqlPersistMPool action)
