-- We need these extensions for servant-generic
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import GHC.Generics (Generic)
import Servant.API.Generic
import Data.Text (Text)
import Servant (Get, Post, PlainText, Capture, JSON, (:>), ReqBody, QueryParam)
import Data.Proxy (Proxy (..))
import Data.Int (Int64)


import API.Types
import API.WithField (WithField)


-- This 'data' definition IS our new API ADT.
-- The 'route' parameter is a placeholder that Servant uses.
data Routes route = Routes
  {
    _putNewFabric 
       :: route 
       :- "fabric" 
       :> ReqBody '[JSON] FabricInfo 
       :> Post '[JSON] (ApiResponse Int64)
  , _getFabricInfo 
       :: route 
       :- "fabric" 
       :> Capture "id" Int64
       :> Get '[JSON] (ApiResponse FullFabric)
  , _getDeliveryPoints
       :: route 
       :- "providers"
       :> Capture "provider" Providers 
       :> "delivery-points" 
       :> QueryParam "city" Text 
       :> Get '[JSON] (ApiResponse [WithField "dpMetros" [Text] DeliveryPoint])
  , _getProviders
       :: route
       :- "providers"
       :> Get '[JSON] (ApiResponse [ProviderInfo])
  , _placeNewOrder
       :: route
       :- "order"
       :> "create"
       :> ReqBody '[JSON] OrderRequest
       :> Post '[JSON] (ApiResponse OrderConfirmationDetails)
  , _setTelegramMessage
       :: route
       :- "order"
       :> "set_telegram_message"
       :> ReqBody '[JSON] SetTelegramMessageRequest
       :> Post '[JSON] (ApiResponse ())
  } deriving (Generic)


tkaniApiProxy :: Proxy (ToServantApi Routes)
tkaniApiProxy = genericApi (Proxy :: Proxy Routes)