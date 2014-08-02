module Handler.Reports where

import Database.Persist.Sql
import Import
import Helpers

getReportsR :: Handler Html
getReportsR = do
    (form, _) <- generateFormPost $ reportForm Nothing
    defaultLayout $ reportsView form

postReportsR :: Handler Html
postReportsR = do
    ((result, form), _) <- runFormPost $ reportForm Nothing

    case result of
        FormSuccess report -> do
            void . runDB $ insert report
            setMessage "Report was created"
            redirect ReportsR

        _ -> do
            defaultLayout $ reportsView form

-- Simple helper for rendering reports form
reportsView :: Widget -> Widget
reportsView form = do
    items <- handlerToWidget $ runDB $ listReportProcess
    [whamlet|
     <h1>These are all the reports
     <ul>
        $forall (Entity _ report, Entity _ process) <- items
            <li>#{show $ report ^. reportTime} #{show $ process ^. processName}

     <form action=@{ReportsR} method="post">^{form}
    |]
