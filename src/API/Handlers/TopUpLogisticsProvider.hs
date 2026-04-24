module API.Handlers.TopUpLogisticsProvider (handler) where


import App (AppM)
import API.Types (TopUpLogisticsProviderReq, ApiResponse)
import Infrastructure.Services.Tinkoff (initiateTinkoffRubleTransfer)

handler :: TopUpLogisticsProviderReq -> AppM (ApiResponse ())
handler _ = undefined