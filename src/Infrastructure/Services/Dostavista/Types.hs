{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Infrastructure.Services.Dostavista.Types where

import Data.Aeson.TH
import Data.Aeson (FromJSON (..), (.:), (.:?), (.!=), withObject, ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import Data.Int (Int64)
import GHC.Generics (Generic)

import Text (camelToSnake, recordLabelModifier)
import Infrastructure.Services.Dostavista.Types.Enums



-- Put this in a types module or at the top of your worker file.
data VehicleType
  = NoVehicleSpecified
  | PassengerCar    -- легковой автомобиль
  | StationWagon    -- универсал / кроссовер
  | CargoVan        -- грузовой фургон
  deriving (Eq, Show)

-- | Contact person details
data DostavistaContact = 
     DostavistaContact
     { contactName  :: Text
     , contactPhone :: Text
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "contact"} ''DostavistaContact)

data DostavistaPackage =
     DostavistaPackage
     { pkgWareCode          :: Text
     , pkgDescription       :: Text
     , pkgItemsCount        :: Double
     , pkgItemPaymentAmount :: Text
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pkg" } ''DostavistaPackage)

-- | Represents a point in the delivery (either pickup or dropoff)
data DostavistaPoint = 
     DostavistaPoint
    { pointAddress               :: Text
     -- Dostavista also needs contact person info for each point
    , pointContactPerson         :: DostavistaContact
     -- Other fields like 'latitude', 'longitude' might be needed
    , pointLatitude              :: Text
    , pointLongitude             :: Text
    , pointRequiredStartDatetime :: Maybe Text
    , pointPackages              :: [DostavistaPackage]
    } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "point", omitNothingFields = True } ''DostavistaPoint)


-- | The main request body for the /create-order endpoint
data DostavistaOrderRequest = 
     DostavistaOrderRequest
     { -- For your model, this will always be "courier"
       drType            :: DostavistaOrderType
     , drMatter          :: Text
     , drVehicleTypeId   :: Int
        -- Total weight in KG
     , drTotalWeightKg   :: Double
        -- List of points. First is pickup (your office), second is dropoff (customer)
     , drPoints          :: [DostavistaPoint]
     , drPaymentMethod   :: DostavistaPayment
     , drInsuranceAmount :: Text
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "dr" } ''DostavistaOrderRequest)

defDostavistaOrderRequest = 
  DostavistaOrderRequest
  { drType            = Standard
  , drMatter          = mempty
  , drVehicleTypeId   = 7
  , drTotalWeightKg   = 0.0
  , drPoints          = []
  , drPaymentMethod   = BALANCE
  , drInsuranceAmount = "5000.00"
  }

data Courier =
     Courier
     { crCourierId :: Int64
     , crName      :: Text
     , crSurname   :: Maybe Text 
     , crPhone     :: Text
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "cr" } ''Courier)

data Order =
     Order
     { orderId       :: Int64
     , status        :: DostavistaOrderStatus
      -- the money value is returned as a String (or Text in Haskell) to avoid floating-point inaccuracies
     , paymentAmount :: Text
     , courier       :: Maybe Courier
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake } ''Order)


-- | The response from the /create-order endpoint
data DostavistaOrderResponse = 
     DostavistaOrderResponse
     { isSuccessful :: Bool
     , order        :: Maybe Order
     } deriving (Show, Eq, Generic)

instance FromJSON DostavistaOrderResponse where
  parseJSON = 
    withObject "DostavistaOrderResponse" $ \o -> do
      isSuccessful <- o .: "is_successful"
      order <- o .:? "order"
      pure DostavistaOrderResponse {..}


data DostavistaOrdersResponse = 
     DostavistaOrdersResponse
     { isSuccessful :: Bool
     , orders       :: [Order]
     } deriving (Show, Eq, Generic)

instance FromJSON DostavistaOrdersResponse where
  parseJSON = 
    withObject "DostavistaOrdersResponse" $ \o -> do
      isSuccessful <- o .: "is_successful"
      orders <- o .: "orders"
      pure DostavistaOrdersResponse {..}


data CancelOrderRequest = 
     CancelOrderRequest
     { orderId :: Int64
     } deriving (Show, Eq, Generic)

instance ToJSON CancelOrderRequest where
  toJSON CancelOrderRequest {..} = object ["order_id" .= show orderId]