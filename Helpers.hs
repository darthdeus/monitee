module Helpers where

import Import hiding ((^.), from, (==.))
import Database.Esqueleto

listReportProcess :: (MonadResource m, MonadSqlPersist m) => m [(Entity Report, Entity Process)]
listReportProcess =
    select . from $ \(r, p) -> do
    where_ (r ^. ReportProcess ==. p ^. ProcessId)
    return (r, p)
