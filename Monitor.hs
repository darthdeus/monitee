module Monitor
       ( runMonitor
       ) where

import Control.Concurrent
import Control.Exception
import qualified Data.Text as T
import Data.Time
import Import
import Network.Wreq as Wreq
import Database.Persist.Postgresql

runMonitor :: PersistConfigPool PersistConf -> IO ()
runMonitor pool = do
    let loop = do
        flip runSqlPersistMPool pool $ do
            processes <- selectList [] [Desc ProcessName]
            mapM_ checkAvailability processes
            return ()

        threadDelay 1000000
        putStrLn "looping 2"
        loop

    loop

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
