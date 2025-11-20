module Infrastructure.Services.Tinkoff.Types where

import Data.Text (Text)
import Data.Int (Int64)


data ApiError = Stub
  deriving Show

-- | Represents the payment status returned by the Tinkoff GetState API.
data Status
  = New              -- ^ Payment session initiated.
  | Confirmed        -- ^ Payment successfully completed and money held.
  | Rejected         -- ^ Payment was rejected by the payment system or issuer.
  | Canceled         -- ^ Payment was canceled by the merchant or user.
  | DeadlineExpired  -- ^ Payment session timed out.
  | Authorized       -- ^ A successful one-step payment or a completed two-step payment.
  | Reversed         -- ^ Payment was fully reversed.
  -- In-progress statuses (can often be handled the same way)
  | Processing       -- ^ A catch-all for intermediate states like FORMSHOWED, AUTHORIZING, etc.
  -- A constructor for any status we don't recognize.
  | Unknown Text
  deriving (Show, Eq)

data OrderDetails = OrderDetails Int64

data PaymentDetails = 
     PaymentDetails 
     { paymentId :: Text
     , orderId :: Text 
     }    
