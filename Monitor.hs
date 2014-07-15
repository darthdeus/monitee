module Monitor where

import Data.Time
import Import
import Database.Persist.Postgresql

runMonitor :: App -> IO ()
runMonitor app = do
    let pool = connPool app

    -- let pingAndUpdate (Entity id process) = do
    --     let url = process ^. processUrl
    --     -- make request
    --     time <- getCurrentTime
    --     insert $ Report time id (undefined :: Bool)



    return ()

withDB :: App -> SqlPersistM a -> IO a
withDB app action = do
    let connStr = pgConnStr $ persistConfig app
    withPostgresqlPool connStr 1 (runSqlPersistMPool action)

loop :: App -> IO [Entity Process]
loop app = do
    withDB app $ do
        selectList [] [Desc ProcessName]

-- devRun :: IO ()
-- devRun = getApplicationDev >>= \(port, app) -> runSettings (setPort port defaultSettings) app

-- connString = "host=localhost dbname=monitee user=darth password= port=5432"

-- runDevDB action = withPostgresqlPool connString 2 (runSqlPersistMPool action)

-- devSettings :: IO (AppConfig DefaultEnv ())
-- devSettings = Yesod.Default.Config.loadConfig (configSettings Development)
