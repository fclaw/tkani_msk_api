{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Domain.Services.Shipping.Yandex (prepareAndSchedulePickup) where


import Katip (logTM, Severity(..), ls)

import App (AppM)


prepareAndSchedulePickup :: AppM Bool
prepareAndSchedulePickup = fmap (const True) $ $(logTM) InfoS "Checking for paid orders to schedule for pickup..."
  -- Get the current date to pass to the query for the idempotency check
