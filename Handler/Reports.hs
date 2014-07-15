module Handler.Reports where

import Database.Persist.Sql
import Import
import Helpers

processOptions :: Handler (OptionList (KeyBackend SqlBackend Process))
processOptions = optionsPersistKey [] [Desc ProcessName] (\x -> toMessage $ x ^. processName)

reportForm :: Html -> MForm Handler (FormResult Report, Widget)
reportForm = renderDivs $ Report
             <$> areq utctimeField "Time" Nothing
             <*> areq (selectField processOptions) "Process Id" Nothing

getReportsR :: Handler Html
getReportsR = do
    (form, _) <- generateFormPost reportForm
    defaultLayout $ reportsView form

postReportsR :: Handler Html
postReportsR = do
    ((result, form), _) <- runFormPost reportForm

    case result of
        FormSuccess report -> do
            void . runDB $ insert report
            setMessage "Report was created"
            redirect ReportsR

        _ -> defaultLayout $ reportsView form

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
