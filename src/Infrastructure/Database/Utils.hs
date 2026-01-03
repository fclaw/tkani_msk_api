{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Infrastructure.Database.Utils (extractValue) where


import Katip
import Data.Text (Text)

import App (AppM)

extractValue :: Either Text a -> (a -> AppM ()) -> AppM ()
extractValue (Right v) app = app v 
extractValue (Left err) _ = $(logTM) ErrorS $ ls $ "db query failed: " <> show err