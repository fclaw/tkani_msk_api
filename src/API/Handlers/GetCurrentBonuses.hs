module API.Handlers.GetCurrentBonuses (handler) where


import Data.Int (Int64)
import Data.Bifunctor (bimap)
import Control.Monad.Reader.Class (ask)


import App (AppM, _appDBPool)
import Infrastructure.Database (getCurrentBonuses)
import API.Types (ApiResponse, CurrentBonusesResp (..), mkError)


handler :: Int64 -> AppM (ApiResponse CurrentBonusesResp)
handler userId = fmap (bimap mkError CurrentBonusesResp) $ fmap _appDBPool ask >>= getCurrentBonuses userId

