{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT)
import Katip (logTM, Severity(..))
import Servant.API.Generic (fromServant, toServant)


import Types (AppM) -- Your custom monad
import API (Routes (..)) -- Import our new ApiWithDocs
import qualified Handlers.Welcome as Welcome

-- This is the implementation of our server.
-- It's a record of handlers that matches the 'Routes' data type.
-- The compiler will enforce that this record has a '_welcome' field
-- with the correct handler type.
apiHandlers :: Routes (AsServerT AppM)
apiHandlers = Routes
  { _welcome = Welcome.handler -- Assign the handler function to the field
  }