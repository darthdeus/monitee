module Helpers where

import qualified Data.Text as T
import           Data.Time hiding (parseTime)
import           Database.Esqueleto
import           Import hiding ((^.), from, (==.))

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

-- TODO - use the value coming from the user
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

listReportProcess :: (MonadResource m, MonadSqlPersist m) => m [(Entity Report, Entity Process)]
listReportProcess =
    select . from $ \(r, p) -> do
    where_ (r ^. ReportProcess ==. p ^. ProcessId)
    return (r, p)

processOptions :: Handler (OptionList (KeyBackend SqlBackend Process))
processOptions = optionsPersistKey [] [Desc ProcessName] (\x -> toMessage $ _processName x)

reportForm :: Maybe Report -> Html -> MForm Handler (FormResult Report, Widget)
reportForm report = renderDivs $ Report
             <$> areq utctimeField "Time" (report ^? _Just.reportTime)
             <*> areq (selectField processOptions) "Process Id" (report ^? _Just.reportProcess)
             <*> areq boolField "Available?" (report ^? _Just.reportStatus)
