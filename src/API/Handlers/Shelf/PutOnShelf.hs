{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module API.Handlers.Shelf.PutOnShelf (handler) where


import Katip
import Data.Int (Int64)
import Data.Text (Text, unlines)
import Control.Monad (when, void)
import Data.Either (fromRight)
import qualified Data.Text as T
import Data.Text as T (unpack, pack)
import Data.Maybe (isNothing, fromJust)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Time (formatTime, defaultTimeLocale)
import qualified Data.HashMap.Strict as HM
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)
import Control.Monad.State.Class (get)
import Control.Concurrent.STM (writeTChan, atomically, readTVar)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.Trans.Except (withExceptT, ExceptT (..), runExceptT, except)

import App
import Infrastructure.Database
import Text (tshow, encodeToText)
import TH.Location (currentModule)
import Infrastructure.Utils.Http (HttpError)
import API.Handlers.PlaceNewOrder(mkInitRequest)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Utils.OrderId (generateOrderId)
import qualified Infrastructure.Services.Tinkoff as Tinkoff
import Infrastructure.Services.Types (PaymentProvider (Tinkoff))
import API.Handlers.PlaceNewOrder(formatOrderItemLine)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, message_id)
import API.Types (ApiResponse, PutOnShelfPaymentOptions (..), mkError)
import qualified Infrastructure.Services.Tinkoff.Types.QR as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Security as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Enum as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Init as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.GetState as Tinkoff


data PutOnShelfError
  = TinkoffHttpError HttpError       -- Failed to create a payment link
  | TinkoffPaymentLinkFailed Text    -- Failed to create a payment link with a textual error
  | TinkoffQrCodeFailed Text         -- Failed to generate QR code
  | DatabaseFailed Text              -- General DB error
  | CapacityExceeded                 -- Shelf capacity exceeded
  | CartEmpty                        -- No items in the cart to put on shelf
  deriving (Show)


wrap action error = withExceptT error (ExceptT action)

handler :: Int64 -> AppM (ApiResponse PutOnShelfPaymentOptions)
handler userId = do
  -- 1. Run the core business logic.
  eResult <- runExceptT (putOnShelf userId)
  -- 2. Pattern match on the result to build the final API response.
  case eResult of
    -- THE SUCCESS CASE
    Right options -> do
      $(logTM) InfoS $ "payment options has been acquired: " <> ls (tshow options)
      -- Return the successful response payload for the bot
      return $ Right options
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

  return PutOnShelfPaymentOptions {..}

