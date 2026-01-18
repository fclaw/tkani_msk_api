{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-} -- ADDED: Needed for ServerT
{-# LANGUAGE MultiParamTypeClasses #-} -- ADDED: Needed for HasServer
{-# LANGUAGE ScopedTypeVariables   #-} -- ADDED: Needed for type annotation on `context`
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lib.Servant.RateLimit (RateLimit, RateLimitState, RateLimitPerIP) where

import Servant
import GHC.TypeLits (Nat)
import Data.Time.Units (TimeUnit)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, encode)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Socket (SockAddr)
import Network.Wai (Request, remoteHost)
import qualified Data.Text as T
import Servant.Server.Internal.Delayed (addAcceptCheck)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Servant.Server.Internal.DelayedIO (withRequest, delayedFailFatal)
import Data.Time.TypeLevel (TimePeriod,  KnownDuration, DurationUnit, durationMicroseconds)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime, nominalDiffTimeToSeconds)


-- The state now stores the time when the next request is allowed for each IP.
type RateLimit = Map SockAddr UTCTime
type RateLimitState = TVar (Map SockAddr UTCTime)

data RateLimitPerIP (delay :: TimePeriod)

-- A custom exception to carry our "time left" value.
data RateLimitExceeded =
     RateLimitExceeded
     { timeLeftSeconds :: Integer -- The number of seconds the user must wait.
     } deriving (Show)

-- We'll create a JSON response for our error.
data RateLimitError =
     RateLimitError
     { error   :: Text
     , retryIn :: Text
     } deriving (Show, Generic)

instance ToJSON RateLimitError

-- New signature! No more exceptions from this function.
checkRateLimit :: NominalDiffTime -> RateLimitState -> Request -> IO (Maybe Integer)
checkRateLimit delay stateTVar req = do
  now <- getCurrentTime
  let clientIp = remoteHost req

  atomically $ do
    stateMap <- readTVar stateTVar
    case Map.lookup clientIp stateMap of
      -- Case 1: IP is in the map.
      Just nextAllowedTime ->
        if now >= nextAllowedTime then do
          -- The user waited long enough. Allow and update the timestamp.
          let newNextTime = addUTCTime delay now
          fmap (const Nothing) $ writeTVar stateTVar (Map.insert clientIp newNextTime stateMap) -- SUCCESS
        else do
          -- The user is too early. Deny and calculate the wait time.
          let timeLeft = ceiling (nextAllowedTime `diffUTCTime` now)
          pure (Just timeLeft) -- FAILURE

      -- Case 2: IP is not in the map (first request).
      Nothing -> do
        let newNextTime = addUTCTime delay now
        fmap (const Nothing) $ writeTVar stateTVar (Map.insert clientIp newNextTime stateMap) -- SUCCESS


-- Our final HasServer instance, based on the library's source code.
instance ( HasServer api context
         , HasContextEntry context RateLimitState
         , KnownDuration delay
         , TimeUnit (DurationUnit delay)
         ) => HasServer (RateLimitPerIP delay :> api) context where

  type ServerT (RateLimitPerIP delay :> api) m = ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

  route Proxy context subserver =
    
    -- Retrieve the state from the context once.
    let
        -- HERE is where we extract the value from the type!
        -- 'durationVal' knows how to read the type-level 'delay'.
        -- We use 'Proxy @delay' to tell it which type to read.
        delayInMicroseconds :: Integer
        delayInMicroseconds = durationMicroseconds @delay
        
        -- Convert to seconds using safe rational division.
        -- This creates a precise fractional number of seconds.
        delaySeconds :: Rational
        delaySeconds = fromInteger delayInMicroseconds / 1000000

        -- Use the standard library function to convert picoseconds to NominalDiffTime.
        rateLimitDelay :: NominalDiffTime
        rateLimitDelay = fromRational delaySeconds
        
        rateLimitState = getContextEntry context

        -- Define the check that will be run on each request.
        -- This uses 'withRequest' to gain access to the 'Request' object.
        rateCheck = withRequest $ \req -> do
          -- Apply our rate-limiting logic.
          maybeSeconds <- liftIO $ checkRateLimit rateLimitDelay rateLimitState req
          
          -- Handle the result.
          case maybeSeconds of
            -- SUCCESS: The request is allowed. Do nothing and let it proceed.
            Nothing -> pure ()
            -- FAILURE: The rate limit was exceeded.
            Just seconds ->
              -- Fail the request fatally using Servant's internal machinery.
              -- This immediately stops routing and sends the specified error.
              delayedFailFatal $ err429
                { errBody = encode $ RateLimitError
                    { Lib.Servant.RateLimit.error = "Rate limit exceeded."
                    , retryIn = T.pack (show seconds) <> " seconds"
                    }
                , errHeaders =
                    -- Include the standard 'Retry-After' header.
                    [ ("Retry-After", LBS.toStrict $ LBS.pack $ show seconds) ]
                }
    in
      -- Attach the check to the server and route it.
      -- Note: The library uses a helper `addAcceptCheck`. This function is
      -- not exported, but we can achieve the same result by applying our
      -- check monadically before running the subserver.
      route (Proxy :: Proxy api) context (addAcceptCheck subserver rateCheck)