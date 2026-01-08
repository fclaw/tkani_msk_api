{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.ReportMonthlySales (handler) where

import App (AppM)
import API.Types (ApiResponse)
import Domain.Services.Reporting (generateAndSendMonthlyReport)

handler :: AppM (ApiResponse ())
handler = fmap Right $ generateAndSendMonthlyReport