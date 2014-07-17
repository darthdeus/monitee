module Handler.Processes where

import Data.Aeson
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Database.Persist.Sql
import Helpers
import Import
import Text.Julius

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

    reports <- runDB $ selectList [ReportProcess ==. processId] [Desc ReportTime, LimitTo 10]

    defaultLayout $(widgetFile "process")

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
