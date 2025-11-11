-- We need these extensions for servant-generic
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import GHC.Generics (Generic)
import Servant.API.Generic
import Data.Text (Text)
import Servant (Get, PlainText, Capture, JSON, (:>))

-- This 'data' definition IS our new API ADT.
-- The 'route' parameter is a placeholder that Servant uses.
data Routes route = Routes
  { -- We define our 'welcome' endpoint as a field in this record.
    -- The ':-' operator is used instead of ':>' inside the record.
    _welcome :: route :- "welcome" :> Get '[PlainText] Text
  } deriving (Generic)
