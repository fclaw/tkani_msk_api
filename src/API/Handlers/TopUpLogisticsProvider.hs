{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}

module API.Handlers.TopUpLogisticsProvider (handler) where


import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.Foldable (for_)
import qualified Data.Text as T
import Control.Monad (void)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL

import Text (tshow, encodeToText)
import Auth (AdminUser)
import App (AppM, _sdekConfig, _yandexConfig, forkAppM, _bankAccount, _appDBPool, ChatKey (MONEY_TRANSFER))
import qualified Infrastructure.Services.Yandex.Config as Ya
import qualified Infrastructure.Services.Sdek.Types.Config as Sdek
import API.Types (TopUpLogisticsProviderReq (..), ApiResponse, Providers (..))
import Infrastructure.Services.Tinkoff (initiateTinkoffRubleTransfer)
import Infrastructure.Services.Tinkoff.Types.RubleTransfer
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Infrastructure.Database (createRubleTransferRecord)

handler :: AdminUser -> TopUpLogisticsProviderReq -> AppM (ApiResponse ())
handler _ req = do
  cfg <- ask
  transferReqId <- fmap (T.pack . toString) $ liftIO nextRandom
  let defTransferReq =
        RubleTransferRequest 
        { rtId      = transferReqId
        , rtFrom    = Payer $ _bankAccount cfg
        , rtTo      = undefined
        , rtPurpose = undefined
        , rtAmount  = tuplAmount req
        }
  let maybeTransferReq =
        case tuplAgent req of
          SDEK   ->
            let sdekCfg = _sdekConfig cfg
            in Just $ defTransferReq 
                { rtTo = Sdek.receiver sdekCfg
                , rtPurpose = Sdek.purpose sdekCfg 
                }
          YANDEX ->
            let yandexCfg = _yandexConfig cfg
            in Just $ defTransferReq 
                { rtTo = Ya.receiver yandexCfg
                , rtPurpose = Ya.purpose yandexCfg 
                }
          _     -> Nothing
  fmap (const (Right ())) $ forkAppM $
    for_ maybeTransferReq $ \transferReq -> do
      resp <- initiateTinkoffRubleTransfer transferReq
      case resp of
        Left err -> send $ tshow err
        Right RubleTransferResponse { rtrError } ->
          case rtrError of
            Nothing -> do 
              send $ 
                "✅ money of " <> 
                tshow (tuplAmount req) <> 
                " transfer initiated for " <>
                tshow (tuplAgent req)
              let pool = _appDBPool cfg  
              void $ createRubleTransferRecord transferReqId (encodeToText (tuplAgent req)) (tuplAmount req) pool
            Just err -> send $ "‼️ \n" <> decodeUtf8 (BL.toStrict (encodePretty err))
              
send msg = void $ sendOrEditTelegramMessage mempty (escapeMarkdownV2 msg) MONEY_TRANSFER Nothing Nothing Nothing