module Handler.Processes where

import Import

getProcessesR :: Handler Html
getProcessesR = do
    processes <- runDB $ selectList [] [Asc ProcessName]
    -- let processes = processName . entityVal <$> result
    defaultLayout $ do
      $(widgetFile "processes")
