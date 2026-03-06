{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}

module Infrastructure.Services.Yandex.Types.Enums where

import Data.Aeson.TH
import Data.Text (unpack)
import Data.Aeson (defaultOptions, SumEncoding(..), FromJSON (..), ToJSON (..), withText)
import GHC.Generics (Generic)
import Data.Char (toLower)

import Text (camelToSnake)


data PickupPointType = Terminal | Warehouse | PickupPoint deriving (Show, Eq)

data PaymentMethod = AlreadyPaid | PostPay | CardOnReceipt | BoundCard deriving (Show, Eq)

instance FromJSON PaymentMethod where
  parseJSON = withText "PaymentMethod" $ \case
    "already_paid"     -> pure AlreadyPaid
    "card_on_receipt"  -> pure CardOnReceipt -- Yandex sometimes alternates these
    "postpay"          -> pure PostPay
    "bound_card"       -> pure BoundCard
    method             -> fail $ "Unknown PaymentMethod: " <> unpack method

instance ToJSON PaymentMethod where
  toJSON method = case method of
    AlreadyPaid     -> "already_paid"
    CardOnReceipt   -> "card_on_receipt"
    PostPay         -> "postpay"
    BoundCard       -> "bound_card"

instance FromJSON PickupPointType where
  parseJSON = withText "PickupPointType" $ \case
    "terminal"      -> pure Terminal
    "warehouse"     -> pure Warehouse
    "pickup_point"  -> pure PickupPoint
    pType             -> fail $ "Unknown PickupPointType: " <> unpack pType

instance ToJSON PickupPointType where
  toJSON pType = case pType of
    Terminal    -> "terminal"
    Warehouse   -> "warehouse"
    PickupPoint -> "pickup_point"
