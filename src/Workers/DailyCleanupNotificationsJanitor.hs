{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Workers.DailyCleanupNotificationsJanitor (runDailyCleanupNotificationsJanitor) where


import Katip
import Data.Foldable (for_)
import Control.Monad.Reader.Class (ask)


import App (AppM, ChatKey (MAIN), _appDBPool)
import Infrastructure.Services.Telegram (deleteMessage)
import Infrastructure.Database (sweepTemporaryNotificationMessages)


runDailyCleanupNotificationsJanitor :: AppM ()
runDailyCleanupNotificationsJanitor = do
  $(logTM) InfoS "DailyCleanupNotificationsJanitor started."
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- sweepTemporaryNotificationMessages pool
  for_ eDbRes $ \messageIds -> do
    $(logTM) InfoS $ 
      "DailyCleanupNotificationsJanitor: \
      \ Deleted temporary notification \
      \ messages with IDs: " <> 
      ls (show messageIds)
    for_ messageIds $ flip deleteMessage MAIN
    