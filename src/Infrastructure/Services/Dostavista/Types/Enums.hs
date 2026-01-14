{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Infrastructure.Services.Dostavista.Types.Enums where

import Data.Aeson.TH
import Data.Aeson (defaultOptions, SumEncoding(..))
import GHC.Generics (Generic)
import Data.Char (toLower)

import Text (camelToSnake)


-- | Статусы заказа (Order Statuses) - The overall status of the delivery order.
data DostavistaOrderStatus
  = New
  | Available
  | Active
  | Completed
  | Reactivated
  | Draft
  | Canceled
  | Delayed
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''DostavistaOrderStatus)


-- | Типы заказа (Order Types)
data DostavistaOrderType
  = Standard
  | SameDay
  deriving (Show, Eq, Generic)


$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''DostavistaOrderType)


-- | Статусы доставки (Delivery Statuses) - The granular status of the courier's journey.
data DostavistaDeliveryStatus
  = DsDraft
  | Planned
  | DsActive
  | Finished
  | DsCanceled
  | DsDelayed
  | CourierAssigned
  | CourierDeparted
  | CourierAtPickup
  | ParcelPickedUp
  | CourierArrived
  | DsDeleted
  | ReturnCourierPickedUp
  | ReturnFinished
  deriving (Show, Eq, Generic)
  
-- Note: 'Ds' prefix added to avoid name clashes with 'DostavistaOrderStatus' (e.g., Active, Canceled).
-- This prefix needs to be stripped by the modifier.
$(deriveJSON defaultOptions 
  { constructorTagModifier = \name ->
      if name == "DsDraft" || 
         name == "DsActive" || 
         name == "DsDeleted" || 
         name == "DsCanceled" || 
         name == "DsDelayed" then
        camelToSnake (drop 2 name)
      else camelToSnake name
    , sumEncoding = UntaggedValue } 
    ''DostavistaDeliveryStatus)


data DostavistaPayment = BALANCE | NONE
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''DostavistaPayment)