module Handler.Processes where

import Database.Persist.Sql
import Import

processForm :: Form Process
processForm = renderDivs $ Process <$> areq textField "Name" Nothing
                                   <*> areq textField "URL" Nothing
                                   <*> areq intField "Monitoring Frequency" (Just 60)

getProcessesR :: Handler Html
getProcessesR = do
    processes <- runDB $ selectList [] [Desc ProcessName]

    (form, _) <- generateFormPost processForm
    defaultLayout $(widgetFile "processes")

postProcessesR :: Handler Html
postProcessesR = do
    ((result, _), _) <- runFormPost processForm

    case result of
        FormSuccess process -> do
            void . runDB $ insert process
            setMessage "Process was added"
            redirect ProcessesR
        _ -> do
            setMessage "Process name is required"
            redirect ProcessesR

getProcessR :: ProcessId -> Handler Text
getProcessR processId = do
    process <- runDB $ get404 processId
    return $ process ^. processName
