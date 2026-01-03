{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Infrastructure.Services.Sdek.Types.Error where


import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data SdekErrorDetail =
     SdekErrorDetail
     { code    :: Text
     , message :: Text
     } deriving (Show, Eq, FromJSON, Generic)

instance ToJSON SdekErrorDetail