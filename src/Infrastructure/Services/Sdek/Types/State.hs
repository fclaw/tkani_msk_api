{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE DeriveAnyClass  #-}

module Infrastructure.Services.Sdek.Types.State where

import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | ================================================================
-- | The Asynchronous Request State ENUM
-- | ================================================================

-- | Represents the state of an asynchronous request, as returned by SDEK.
data SdekRequestState
  = Accepted     -- "ACCEPTED":   Request is valid and queued for processing.
  | Waiting      -- "WAITING":    Request is waiting for another request to complete.
  | Successful   -- "SUCCESSFUL": The entity (e.g., order) was successfully created.
  | Invalid      -- "INVALID":    The request failed deep validation.
  | UnknownState Text -- A catch-all for any future statuses SDEK might add.
  deriving (Show, Eq, Generic)

-- To parse this enum from SDEK's JSON strings, we need a custom FromJSON instance.
instance FromJSON SdekRequestState where
  parseJSON = withText "SdekRequestState" $ \t ->
    pure $ case t of
      "ACCEPTED"   -> Accepted
      "WAITING"    -> Waiting
      "SUCCESSFUL" -> Successful
      "INVALID"    -> Invalid
      -- This catch-all makes our parsing robust against API changes.
      other        -> UnknownState other

-- to meet the derivation of SdekConfirmation
instance ToJSON SdekRequestState where
  toJSON _ = undefined

convertStateToSql :: SdekRequestState -> Text
convertStateToSql Accepted     = "accepted"
convertStateToSql Waiting      = "waiting"
convertStateToSql Successful   = "successful"
convertStateToSql Invalid      = "invalid"
convertStateToSql (UnknownState t) = error $ "Unknown SdekRequestState: " <> show t


data SdekReceiptState
     = ACCEPTED
     | INVALID      
     | WAITING      
     | SUCCESSFUL
     | OTHER
     deriving (Show, Eq, Generic, FromJSON, ToJSON)