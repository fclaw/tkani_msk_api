{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}


module API.Handlers.Shelf.PutOnShelf (handler) where


import Katip
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Text (Text, unlines, pack)
import Control.Monad (when, void, join)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Foldable (for_)
import Data.Bifunctor (first, bimap)
import qualified Data.Text as T
import Data.Text as T (unpack, pack)
import Data.Maybe (isNothing, fromJust, isJust)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Time (formatTime, defaultTimeLocale)
import qualified Data.HashMap.Strict as HM
import Network.Wreq (postWith, defaults, manager, responseBody)
import Control.Lens ((&), (.~), (^.))
import qualified Data.Map.Strict as M
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)
import Control.Monad.State.Class (get)
import Control.Exception (SomeException, try)
import Data.Aeson ((.=), object, eitherDecode, Value (Null))
import Control.Concurrent.STM (writeTChan, atomically, readTVar)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.Trans.Except (withExceptT, ExceptT (..), runExceptT, except)

import App
import Infrastructure.Database
import Text (tshow, encodeToText)
import TH.Location (currentModule)
import Infrastructure.Utils.Http (HttpError)
import Workers.SimpleOrderOrchestrator.Sdek (mkInitRequest, formatOrderItemLine)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Utils.OrderId (generateOrderId)
import qualified Infrastructure.Services.Tinkoff as Tinkoff
import Infrastructure.Database (storeTelegramMessageDetails, TelegramMessageDetails (..))
import Infrastructure.Services.Types (PaymentProvider (Tinkoff))
import qualified Infrastructure.Services.Tinkoff.Types.QR as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Security as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Enum as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Init as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.GetState as Tinkoff
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, message_id, ParseMode(MarkdownV2), MessageIdResponse (..), disableLinkPreviewOption)
import API.Types (ApiResponse, PutOnShelfPaymentOptions (..), mkError, ShelfStatus (..), mkDefPutOnShelfPaymentOptions, PutOnShelfRequest (..))

data PutOnShelfError
  = TinkoffHttpError HttpError       -- Failed to create a payment link
  | TinkoffPaymentLinkFailed Text    -- Failed to create a payment link with a textual error
  | TinkoffQrCodeFailed Text         -- Failed to generate QR code
  | DatabaseFailed Text              -- General DB error
  | CapacityExceeded                 -- Shelf capacity exceeded
  | CartEmpty                        -- No items in the cart to put on shelf
  deriving (Show)


wrap action error = withExceptT error (ExceptT action)

handler :: Int64 -> PutOnShelfRequest -> AppM (ApiResponse ())
handler userId PutOnShelfRequest {posrChatId=chatId} = do
  -- 1. Run the core business logic.
  eResult <- runExceptT (putOnShelf userId)
  -- 2. Pattern match on the result to build the final API response.
  case eResult of
    -- THE SUCCESS CASE
    Right options -> do
      $(logTM) InfoS $ "payment options has been acquired: " <> ls (tshow options)
      -- Return the successful response payload for the bot
      fmap Right $ forkAppM $ do 
        eMessageId <- sendMessage chatId options
        case eMessageId of
          Left err -> do
            $(logTM) ErrorS $ "Failed to send Telegram message for payment link: " <> ls (show err)
          Right maybeMessageId ->
            for_ maybeMessageId $ \msgIdResp -> do
              let MessageIdResponse{..} = msgIdResp
              $(logTM) InfoS $ "Successfully sent Telegram message for payment link, message ID: " <> ls (show msgIdResp)
              pool <- fmap _appDBPool ask
              let details =
                    TelegramMessageDetails
                    { tmdSingleOrderId = Nothing
                    , tmdShelfOrderId  = pspoOrderId options
                    , tmdChatId        = chatId
                    , tmdMessageId     = message_id
                    }
              void $ storeTelegramMessageDetails details pool
    -- THE FAILURE CASES
    Left err -> do
      case err of
        CapacityExceeded -> do
          $(logTM) ErrorS $ "Shelf capacity exceeded for userId: " <> ls (tshow userId)
          return $ Left $ mkError "Shelf capacity exceeded."
        CartEmpty -> do
          $(logTM) ErrorS $ "Cart is empty for userId: " <> ls (tshow userId)
          return $ Left $ mkError "No items in the cart to put on shelf."
        _ -> do
          -- Log the specific internal error
          $(logTM) ErrorS $ "Failed to obtain payment options: " <> ls (show err)
          -- Return a user-friendly, generic failure response
          return $ Left $ mkError "Failed to obtain payment options. See server logs for details."  

putOnShelf :: Int64 -> ExceptT PutOnShelfError AppM PutOnShelfPaymentOptions
putOnShelf userId = do
  cfg <- ask
  let pool = _appDBPool cfg
  shelfStatus <- wrap(getShelfStatus userId pool) DatabaseFailed
  case shelfStatus of
    Active     -> onSuccess userId
    status     -> return $ mkDefPutOnShelfPaymentOptions { pspoShelfStatus = status }


onSuccess :: Int64 -> ExceptT PutOnShelfError AppM PutOnShelfPaymentOptions
onSuccess userId = do
  cfg <- ask
  let pool = _appDBPool cfg
    -- fetch items
  maybeDetails <- wrap (getPutOnDShelfDetails userId pool) DatabaseFailed

  when (isNothing maybeDetails) $ except $ Left CartEmpty
  let PutOnShelfDetails {..} = fromJust maybeDetails

  -- check capacity overflow
  let totalCount = length posdItems + fromIntegral posdItemsOnShelfCount
  when(totalCount > fromIntegral (_shelfCapacity cfg)) $
    except $ Left $ CapacityExceeded


  orderId <- fmap ((<>) "SHELF-") $ liftIO generateOrderId
  -- prepare Tinkoff init request
  let tinkoffCred = _tinkoffCred cfg
  let initReq = mkInitRequest orderId posdItems posdPhone tinkoffCred

  $(logTM) InfoS $ ls $ "API.Handlers.Shelf.PutOnShelf:initReq " <> encodePretty initReq
  tinkoffResp :: Tinkoff.InitResponse <- wrap (Tinkoff.initiateTinkoffPayment initReq) TinkoffHttpError

  when (Tinkoff.irSuccess tinkoffResp /= True) $
    except $ Left $ TinkoffPaymentLinkFailed (fromJust $ Tinkoff.irMessage tinkoffResp)

  paymentLink <- wrap ( 
    case Tinkoff.irPaymentURL tinkoffResp of
      Just link  -> pure (Right link)
      Nothing -> pure (Left ())
    ) (const $ TinkoffPaymentLinkFailed "Tinkoff Init API did not return a payment URL.")

  let tinkoffPaymentId = fromJust (Tinkoff.irPaymentId tinkoffResp)

  let qrReq = 
        Tinkoff.defGetQrRequest 
        { Tinkoff.gqrTerminalKey = tinkoffTerminalKey tinkoffCred
        , Tinkoff.gqrPaymentId = read @Int64 (T.unpack tinkoffPaymentId)
        , Tinkoff.gqrToken =
           Tinkoff.generateGetQrToken $
              Tinkoff.GetQrToken
              tinkoffPaymentId
              (tinkoffTerminalKey tinkoffCred)
              (tinkoffSecret tinkoffCred)
              Tinkoff.PAYLOAD
        }

  $(logTM) InfoS $ ls $ "QR req: " <> encodePretty qrReq
  tinkoffQrResp :: Tinkoff.GetQrResponse <- wrap (Tinkoff.getTinkoffQRCode qrReq) TinkoffHttpError   

  when(Tinkoff.gqrrSuccess tinkoffQrResp == False) $
    $(logTM) ErrorS $ "Tinkoff QR fails. " <> ls (show tinkoffQrResp)

  let linkToQr = Tinkoff.gqrrData tinkoffQrResp

  when (isNothing linkToQr) $
    except $ Left $ TinkoffQrCodeFailed "Tinkoff Get QR API did not return a QR code link."

  let amount = sum $ map oiTotalPrice posdItems  -- convert from kopecks to rubles
  let totalPrice = amount

  let newPaymentRecord =
        NewPaymentRecord
        { nprOrderId           = Nothing
        , nprProvider          = Tinkoff
        , nprProviderPaymentId = tinkoffPaymentId
        , nprAmountKopecks     = round amount
        , nprPaymentUrl        = paymentLink
        , nprError             = Nothing
        , nprToken             = Tinkoff.irToken initReq
        , nprPaymentFlow       = encodeToText PutOnShelf
        , nprShelfOrderId      = Just orderId
        }
  
  -- Generate a notification message ID placeholder (could be from Telegram)
  tz <- liftIO getCurrentTimeZone
  tm <- currentTime
  let localTime = utcToLocalTime tz tm
  let timeStr = pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" localTime
  let itemLines = map formatOrderItemLine posdItems
  let itemsBlock = T.unlines itemLines
  let templateData = 
       HM.fromList 
       [ ("orderId", orderId)
       , ("timestamp", timeStr)
       , ("shelfId", tshow posdShelfId)
       , ("customerName", posdUserInitials)
       , ("customerPhone", posdPhone)
       , ("itemCount", tshow (length posdItems))
       , ("itemsBlock", itemsBlock)
       ]
  message <- fmap escapeMarkdownV2 $ render $currentModule templateData
  eTelResp <- lift $ sendOrEditTelegramMessage mempty message SHELF Nothing Nothing Nothing
  let notificationId = fromRight 0 $ fmap message_id eTelResp

  -- Finalize the entire "put on shelf" checkout process within a single database transaction.
  -- This involves three critical steps:
  --   1. Create the 'shelf_order' record.
  --   2. Create the associated 'payment' record.
  --   3. Clear all items from the user's cart.
  -- The entire block is transactional: if any step fails, all previous steps are rolled back,
  -- ensuring the database remains in a consistent state. 'wrap' handles any database
  -- exception by converting it into our application-specific 'DatabaseFailed' error.
  void $ wrap (finalizeShelfCheckout userId orderId notificationId newPaymentRecord pool) DatabaseFailed

  let getStateRequest = 
        Tinkoff.GetStateRequest
        { gsrqPaymentId = tinkoffPaymentId
        , gsrqToken = 
            Tinkoff.generateGetStateToken $
              Tinkoff.GetStateToken
              tinkoffPaymentId
              (tinkoffTerminalKey tinkoffCred)
              (tinkoffSecret tinkoffCred)
        , gsrqTerminalKey = tinkoffTerminalKey tinkoffCred
        , gsrqIP = Nothing
        }

  st <- lift get
  liftIO $ atomically $ readTVar st >>= ((`writeTChan` (PutOnShelf, orderId, getStateRequest)) . _tinkoffPaymentChan)
  let putOnShelfPaymentOptions = 
       PutOnShelfPaymentOptions
       { pspoPaymentLink = Just paymentLink 
       , pspoTotalPrice  = Just totalPrice
       , pspoLinkToQr    = linkToQr
       , pspoOrderId     = Just orderId
       , pspoShelfStatus = Active
       }
  return putOnShelfPaymentOptions


sendMessage :: Int64 -> PutOnShelfPaymentOptions -> AppM (Either Text (Maybe MessageIdResponse))
sendMessage chatId PutOnShelfPaymentOptions {..} = do
  bots <- fmap _bots ask
  let (bot,_) = (M.!) bots MAIN
  let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendMessage"
  httpManager <- fmap _configHttpManager ask
  eTelResp <-
    case pspoShelfStatus of
      Active -> do
        let templateData = HM.fromList [("amount", tshow (fromJust pspoTotalPrice))]
        message <- fmap escapeMarkdownV2 $ render ($currentModule <> "." <> tshow Active) templateData
        let buttons = 
              object [
                "inline_keyboard" .=
                  [[ object 
                    [ "text" .= ("💳 Оплатить картой" :: Text)
                    , "url"  .= fromJust pspoPaymentLink
                    ]
                  ],
                  [if isJust pspoLinkToQr then
                      object 
                      [ "text" .= ("📱 Оплатить СПБ" :: Text)
                      , "url"  .= fromJust pspoLinkToQr
                      ]
                    else Null
                  ]
                  ]
                ]
        let payload =
              object
              [ "chat_id"              .= chatId
              , "text"                 .= message
              , "parse_mode"           .= tshow MarkdownV2
              , "link_preview_options" .= 
                  disableLinkPreviewOption
              , "reply_markup"         .= buttons
              ]
        liftIO $ try @SomeException $ fmap Just $ postWith (defaults & manager .~ Right httpManager) url payload
      status -> do
        messageText <- fmap escapeMarkdownV2 $ render ($currentModule <> "." <> tshow status) HM.empty
        let payload =
              object
              [ "chat_id"              .= chatId
              , "text"                 .= messageText
              , "parse_mode"           .= T.pack (show MarkdownV2)
              ]
        liftIO $ try @SomeException $ fmap (const Nothing) $ postWith (defaults & manager .~ Right httpManager) url payload
  case eTelResp of
    Left err            -> return $ Left (tshow err)
    Right maybeResponse -> return $ sequenceA $ maybeResponse <&> \response -> first pack (eitherDecode @MessageIdResponse (response ^. responseBody))
  