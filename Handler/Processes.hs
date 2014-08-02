module Handler.Processes where

import Data.Aeson
-- import Data.Time
import Database.Persist.Sql
import Import
import Prelude (head)

processForm :: Maybe Process -> Form Process
processForm p = renderDivs $ Process <$> areq textField "Name" (p ^? _Just.processName)
                                     <*> areq textField "URL" (p ^? _Just.processUrl)
                                     <*> areq intField "Monitoring Frequency" (Just 60)

getProcessesR :: Handler Html
getProcessesR = do
    processes <- runDB $ selectList [] [Desc ProcessName]

    (form, _) <- generateFormPost $ processForm Nothing
    defaultLayout $(widgetFile "processes")

postProcessesR :: Handler Html
postProcessesR = do
    ((result, form), _) <- runFormPost $ processForm Nothing

    case result of
        FormSuccess process -> do
            void . runDB $ insert process
            setMessage "Process was added"
            redirect ProcessesR
        _ -> do
            processes <- runDB $ selectList [] [Desc ProcessName]
            defaultLayout $(widgetFile "processes")

getProcessR :: ProcessId -> Handler Html
getProcessR processId = do
    process <- runDB $ get404 processId
    (form, _) <- generateFormPost $ processForm $ Just process

    reports <- runDB $ selectList [ReportProcess ==. processId] [Desc ReportTime, LimitTo 10000]

    --let availability = neco reports

    defaultLayout $(widgetFile "process")

-- neco :: [Entity Report] -> [(UTCTime, Bool)]
-- neco entities = map (\report -> (report ^. reportTime, report ^. reportStatus)) reports
--     where reports = map entityVal entities

postProcessR :: ProcessId -> Handler Html
postProcessR processId = do
    process <- runDB $ get404 processId

    ((result, _), _) <- runFormPost $ processForm $ Just process

    case result of
        FormSuccess newProcess -> do
            runDB $ replace processId newProcess
            setMessage "Process was updated"
            redirect $ ProcessR processId
        _ -> do
            setMessage "Invalid process"
            redirect $ ProcessR processId

getDashboardR :: Handler Html
getDashboardR = do
   processes <- runDB $ selectList [] [Desc ProcessName]
   defaultLayout $(widgetFile "dashboard")
