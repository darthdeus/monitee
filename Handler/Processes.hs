module Handler.Processes where

import qualified Data.Text as T
import           Data.Time hiding (parseTime)
import           Database.Persist.Sql
import           Import
import           Text.Read hiding (lift)

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

utctimeFieldView :: Text -> Text -> [(Text, Text)] -> Either Text UTCTime -> Bool -> WidgetT App IO ()
utctimeFieldView idAttr name other result isRequired =
    [whamlet|
       <input id="#{idAttr}" name="#{name}" *{other} isRequired:required type="date">
       <input id="#{idAttr}" name="#{name}" *{other} isRequired:required type="text" placeholder="HH:MM">
     |]

-- TODO - use form's parseDate
parseDayTime :: Text -> Text -> Either (SomeMessage App) (Maybe UTCTime)
parseDayTime dayText timeText = case (readMaybe $ T.unpack dayText :: Maybe Day) of
    Nothing -> Left "invalid date format"
    Just day -> case parseTime timeText of
        Left _ -> Left "invalid time format"
        Right time -> Right . Just $ UTCTime day (timeOfDayToTime time)

processForm :: Html -> MForm Handler (FormResult Process, Widget)
processForm = renderDivs $ Process <$> areq textField "Name" Nothing

processIdField :: Field Handler ProcessId
processIdField = undefined

--processOptions :: Handler (OptionList (KeyBackend SqlBackend (ProcessGeneric SqlBackend)))
--processOptions :: Handler (OptionList (KeyBackend SqlBackend Process))
--processOptions = optionsPersist [] [Desc ProcessName] id

reportForm :: Html ->  MForm Handler (FormResult Report, Widget)
reportForm = renderDivs $ Report
             <$> areq utctimeField "Time" Nothing
            -- <$> (lift $ liftIO getCurrentTime)
            -- <*> areq (selectField processOptions) "Process Id" Nothing
             <*> areq (selectField pairs) "Process Id" Nothing
             where pairs = do
                       items <- runDB $ selectList [] []
                       optionsPairs $ map (\p -> (entityVal p ^. processName, entityKey p)) items

getProcessesR :: Handler Html
getProcessesR = do
    processes <- runDB $ selectList [] [Desc ProcessName]

    (form, enctype) <- generateFormPost processForm
    defaultLayout $(widgetFile "processes")

postProcessesR :: Handler Html
postProcessesR = do
    ((result, _), _) <- runFormPost processForm

    case result of
        FormSuccess process -> do
            void . runDB $ insert process
            setMessage "Process was addeds"
            redirect ProcessesR
        _ -> do
            setMessage "Process name is required"
            redirect ProcessesR

getProcessR :: ProcessId -> Handler Text
getProcessR processId = do
    process <- runDB $ get404 processId
    return $ process ^. processName


getReportsR :: Handler Html
getReportsR = do
    (form, _) <- generateFormPost reportForm
    reportsView form

postReportsR :: Handler Html
postReportsR = do
    ((result, form), _) <- runFormPost reportForm

    case result of
        FormSuccess _ -> do
            setMessage "Result was created"
            redirect ReportsR

        _ -> reportsView form

-- Simple helper for rendering reports form
reportsView form =
    defaultLayout [whamlet|
                   <form action=@{ReportsR} method="post">
                                                   ^{form}
                          |]
