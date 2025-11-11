{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.Welcome(handler) where

import Types (AppM)
import Data.Text (Text, unpack)
import Katip (logTM, Severity(..))
import GHC.Exts (fromString)



-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: AppM Text
handler = do
  let name = "sadcx"
  $(logTM) InfoS $ "Welcome handler (generic) called for: " <> fromString (unpack name)
  return $ "Hello, " <> name <> "! (from a generic API)"
