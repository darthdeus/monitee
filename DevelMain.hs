-- | Development version to be run inside GHCi.

module DevelMain where

import Control.Applicative
import Data.Monoid
import Prelude
import Application (getApplicationRepl)

import Control.Exception (finally)
import Control.Concurrent
import Data.IORef
import Foreign.Store
import Network.Wai.Handler.Warp

import Foundation
import Debug.Trace

update :: IO ()
update = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> do
          done <- storeAction doneStore newEmptyMVar
          tid <- start done
          _ <- storeAction (Store tidStoreNum) (newIORef tid)
          return ()
      -- server is already running
      Just tidStore ->
          -- shut the server down with killThread and wait for the done signal
          modifyStoredIORef tidStore $ \tid -> do
              trace ("killling " ++ show tid) $ killThread tid
              withStore doneStore takeMVar >> readStore doneStore >>= start
  where
    doneStore = Store 0
    tidStoreNum = 1

    modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
    modifyStoredIORef store f = withStore store $ \ref -> do
        v <- readIORef ref
        f v >>= writeIORef ref

-- | Start the server in a separate thread.
start :: MVar () -- ^ Written to when the thread is killed.
      -> IO ThreadId
start done = do
    (port,site,app) <- getApplicationRepl

    let monitorId = monitorThread site

    appTid <- forkIO (finally (runSettings (setPort port defaultSettings) app)
                     (putMVar done () >> killThread (traceShow ("Killed a monitor thread " ++ show monitorId) monitorId)))

    return appTid


-- | Start or restart the server.
update2 :: IO ()
update2 = do
    mtidStore <- lookupStore tid_1
    case mtidStore of
      Nothing -> do
          done <- newEmptyMVar
          _done_0 <- newStore done
          tid <- start done
          tidRef <- newIORef tid
          _tid_1 <- newStore tidRef
          return ()
      Just tidStore -> do
          tidRef <- readStore tidStore
          tid <- readIORef tidRef
          done <- readStore (Store done_0)
          killThread tid
          takeMVar done

          newTid <- start done
          writeIORef tidRef newTid
  where tid_1 = 1
        done_0 = 0
