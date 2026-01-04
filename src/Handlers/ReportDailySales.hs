{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.ReportDailySales (handler) where

import App (AppM)
import API.Types (ApiResponse)
import Domain.Services.Reporting (generateAndSendDailyReport)

handler :: AppM (ApiResponse ())
handler = fmap Right $ generateAndSendDailyReport