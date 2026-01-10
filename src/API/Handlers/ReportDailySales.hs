{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.ReportDailySales (handler) where

import App (AppM)
import API.Types (ApiResponse)
import Domain.Services.Reporting (generateAndSendDailyReport)

handler :: AppM (ApiResponse ())
handler = fmap Right $ generateAndSendDailyReport