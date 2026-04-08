{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Workers.FabricLifecycleManager (runFabricLifecycleManager) where


import Katip
import Data.Int (Int32)
import Data.Foldable (for_)
import Data.Time (utctDay, Day)
import Data.Time.Calendar (diffDays)
import Control.Monad.Reader.Class (ask)

import Text (tshow)
import Domain.Warehouse.Enums (FabricLifecycle (..))
import App (AppM, _appDBPool, _thresholdMetres, currentTime)
import Infrastructure.Database (fetchFabricLifeCycleInfo, updateFabricLifecycle, FabricLifeCycleInfo (..))


runFabricLifecycleManager :: AppM ()
runFabricLifecycleManager = do
  $(logTM) InfoS "FabricLifecycleManager started."
  cfg <- ask
  let pool = _appDBPool cfg
  let minAvailableLength = _thresholdMetres cfg
  eDbRes <- fetchFabricLifeCycleInfo minAvailableLength pool
  case eDbRes of
    Left err -> $(logTM) ErrorS $ ls $ "Error fetching fabric lifecycle info: " <> err
    Right xs -> do
      -- Here you would implement the logic to update the lifecycle of fabrics based on the fetched information.
      -- This might involve checking the current lifecycle status of each fabric, comparing it with the fetched info,
      -- and then updating the database accordingly. You might also want to log any changes made to the lifecycle status.
      $(logTM) InfoS $ ls $ "Successfully fetched fabric lifecycle info: " <> tshow xs
      currDay <- fmap utctDay currentTime
      for_ xs $ \info@FabricLifeCycleInfo {..} -> do
        -- Implement your lifecycle update logic here, for example:
        -- if info.lifecycle == OnSale && currDay > someDate then update lifecycle to Clearance
        -- You would also want to log any lifecycle changes made.
        $(logTM) InfoS $ ls $ "Processing fabric info: " <> tshow info
        let maybeNewCycle = checkTransition fcliLifecycle fcliSince currDay
        for_ maybeNewCycle $ \(cycle, discount) -> do
          -- Update the lifecycle in the database and log the change
          $(logTM) InfoS $ ls $ 
            "Updating lifecycle for fabric " <> 
            tshow fcliId <> "(" <> tshow fcliFabricType  <> 
            ") from " <> tshow fcliLifecycle <> " to " <> tshow cycle
          -- Here you would call a function to update the lifecycle in the database, e.g.:
          updateFabricLifecycle fcliId fcliFabricType cycle discount pool


checkTransition :: FabricLifecycle -> Day -> Day -> Maybe (FabricLifecycle, Int32)
checkTransition Regular since today 
  | diffDays today since >= 30 = Just (OnSale, 30) -- Example: after 30 days, move to OnSale with a 30% discount
checkTransition OnSale since today 
  | diffDays today since >= 30 = Just (Clearance, 50) -- Example: after 30 days, move to Clearance with a 50% discount
checkTransition Clearance since today = Nothing -- Example: Clearance is the final stage, no further transitions
checkTransition _ _ _ = Nothing