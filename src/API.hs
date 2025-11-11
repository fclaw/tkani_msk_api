-- We need these extensions for servant-generic
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import GHC.Generics (Generic)
import Servant.API.Generic
import Data.Text (Text)
import Servant (Get, Post, PlainText, Capture, JSON, (:>), ReqBody)

import API.Types (FabricInfo, ApiResponse)
import API.WithField (WithField)


-- This 'data' definition IS our new API ADT.
-- The 'route' parameter is a placeholder that Servant uses.
data Routes route = Routes
  {
    _putNewFabric :: route :- "fabric" :> ReqBody '[JSON] FabricInfo :> Post '[JSON] (ApiResponse Int)
  , _getFabricInfo :: route :- "fabric" :> Capture "id" Int :> Get '[JSON] (ApiResponse (WithField "fiId" Int FabricInfo))
  } deriving (Generic)
