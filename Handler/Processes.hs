module Handler.Processes where

import qualified Data.Text as T
import           Data.Time hiding (parseTime)
import           Database.Persist.Sql
import           Import
import Helpers

utctimeField :: Field Handler UTCTime
utctimeField = Field
    { fieldParse = parseUtcInput
    , fieldView = utctimeFieldView
    , fieldEnctype = UrlEncoded
    }

parseUtcInput :: [Text] -> [FileInfo] -> Handler (Either (SomeMessage App) (Maybe UTCTime))
parseUtcInput [] _         = return $ Right Nothing
parseUtcInput [day,time] _ = return $ parseDayTime day time
parseUtcInput _ _          = return $ Left "Invalid form submission. Both date and time are required."

utctimeFieldView :: Text -> Text -> [(Text, Text)] -> Either Text UTCTime -> Bool -> Widget
utctimeFieldView idAttr name other _ isReq =
    [whamlet|
       <input id="#{idAttr}" name="#{name}" *{other} :isReq:required="" type="date">
       <input id="#{idAttr}" name="#{name}" *{other} :isReq:required="" type="text" placeholder="HH:MM">
     |]

parseDayTime :: Text -> Text -> Either (SomeMessage App) (Maybe UTCTime)
parseDayTime dayText timeText = case parseDate $ T.unpack dayText of
    Left _ -> Left "invalid date format"
    Right day -> case parseTime timeText of
        Left _ -> Left "invalid time format"
        Right time -> Right . Just $ UTCTime day (timeOfDayToTime time)

processForm :: Html -> MForm Handler (FormResult Process, Widget)
processForm = renderDivs $ Process <$> areq textField "Name" Nothing

--processOptions :: Handler (OptionList (KeyBackend SqlBackend (ProcessGeneric SqlBackend)))
processOptions :: Handler (OptionList (KeyBackend SqlBackend Process))
processOptions = optionsPersistKey [] [Desc ProcessName] (\x -> toMessage $ x ^. processName)

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
