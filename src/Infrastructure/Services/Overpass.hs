{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Infrastructure.Services.Overpass (fetchAllRussianMetros) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM)
import Katip
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Control.Monad.Reader.Class (ask)

import App (AppM, _configHttpManager)
import Infrastructure.Services.Overpass.Types
import Infrastructure.Utils.Http


showt = pack . show

-- | Fetches metros for ALL configured cities one by one.
fetchAllRussianMetros :: AppM [MetroStation]
fetchAllRussianMetros = do
    $(logTM) InfoS "Starting sequential Metro download..."
    
    -- "forM" loops sequentially (single threaded)
    -- This guarantees we never hit the Rate Limit.
    resultsOfLists <- forM metroCities $ \city -> do
        
        let (minLat, minLon, maxLat, maxLon) = ctBbox city
        
        -- 1. Build Query for this city
        -- node["station"="subway"](SOUTH, WEST, NORTH, EAST);
        let query = "[out:json];node[\"station\"=\"subway\"]"
                    <> "(" <> showt minLat <> "," <> showt minLon <> "," 
                    <> showt maxLat <> "," <> showt maxLon <> ");out;"
        
        let params = [("data", query)]

        -- 2. Call API
        cfg <- ask
        let httpManager = _configHttpManager cfg
        resp <- withRetry 3 $ getReq @OverpassResponse httpManager "https://overpass-api.de/api/interpreter" params [] Nothing
        let metros = extractOverpassMetros resp
        $(logTM) InfoS $ ls $ "Fetched " <> pack (show (length metros)) <> " stations for " <> ctName city
                
        -- 3. SLEEP for 1 second to be polite/safe
        liftIO $ threadDelay 1000000 
        
        return metros

    -- 4. Flatten [[Station]] -> [Station]
    let allStations = concat resultsOfLists
    $(logTM) InfoS $ ls $ "Total Metro Stations Cached: " <> pack (show (length allStations))
    
    return allStations