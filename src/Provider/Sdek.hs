module Provider.Sdek (getDeliveryPoints) where

import Data.Text (Text)
import Control.Monad.IO.Class


import Types (AppM)
import API.Types (ApiResponse, DeliveryPoint (..))
import Utils.Http
import Provider.Sdek.Auth (getValidSdekToken)


getDeliveryPoints :: Text -> AppM (ApiResponse [DeliveryPoint])
getDeliveryPoints city = do 
  token <- getValidSdekToken
  liftIO $ print token
  -- try r requesting points
  -- if token is expired request new one , update token in state
  -- if prev step fails request again
