{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.Announcement.Draft (handler) where

import Katip

import App (AppM)
import API.Types (ApiResponse)

handler :: AppM ()
handler = $(logTM) InfoS $ ls @String $ "announcement is being drafted.."