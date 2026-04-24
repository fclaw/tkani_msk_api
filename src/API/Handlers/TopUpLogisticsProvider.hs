module API.Handlers.TopUpLogisticsProvider (handler) where


import App (AppM)
import API.Types (TopUpLogisticsProviderReq, ApiResponse)

handler :: TopUpLogisticsProviderReq -> AppM (ApiResponse ())
handler _ = undefined