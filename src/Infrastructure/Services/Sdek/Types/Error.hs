{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}


module Infrastructure.Services.Sdek.Types.Error where


import Data.Text (Text, unpack)
import Data.Aeson (FromJSON (..), ToJSON (..), withText, Value(String))
import GHC.Generics (Generic)


data ErrorCode = TariffUnsupported | Other Text
  deriving (Show, Eq, Generic)

instance FromJSON ErrorCode where
  parseJSON = withText "ErrorCode" $ \code -> 
     case code of
       "err_result_service_empty" -> pure TariffUnsupported
       _ -> pure (Other code)


instance ToJSON ErrorCode where
  toJSON TariffUnsupported = String "err_result_service_empty"
  toJSON (Other code) = String code


data SdekErrorDetail =
     SdekErrorDetail
     { code    :: ErrorCode
     , message :: Text
     } deriving (Show, Eq, FromJSON, Generic)

instance ToJSON SdekErrorDetail