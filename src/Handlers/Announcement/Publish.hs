{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.Announcement.Publish (handler) where

import Katip

import App (AppM)
import API.Types (ApiResponse)

handler :: AppM ()
handler = $(logTM) InfoS $ ls @String $ "announcement is about to be published.."