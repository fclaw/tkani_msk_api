{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module API.Handlers.Shelf.SaveShelfSubmissionInfo (handler) where


import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool)
import Infrastructure.Database (saveShelfSubmissionInfo)
import API.Types(ApiResponse, ShelfSubmissionChatDetails (..))


handler :: ShelfSubmissionChatDetails -> AppM (ApiResponse ())
handler submission = fmap _appDBPool ask >>= (fmap (const (Right ())) . saveShelfSubmissionInfo submission)