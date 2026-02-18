{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Workers.FabricLifecycleManager (runFabricLifecycleManager) where


import Katip
import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool)
-- import Infrastructure.Database (fetchFabricLifeCycleInfo)


runFabricLifecycleManager :: AppM ()
runFabricLifecycleManager = do
  $(logTM) InfoS "FabricLifecycleManager started."
  cfg <- ask
  let pool = _appDBPool cfg
--   eDbRes <- fetchFabricLifeCycleInfo pool
  undefined
