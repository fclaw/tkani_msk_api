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

module Lib.Servant.RateLimit (RateLimitState, RateLimitPerUser, State (..)) where

import Servant
import Data.Int (Int64)
import Control.Exception (handle, Exception, throwIO)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Time.Units (TimeUnit)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, encode)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai (Request, remoteHost, queryString)
import Network.HTTP.Types.URI (queryToQueryText)
import qualified Data.Text as T
import Servant.Server.Internal.Delayed (addAcceptCheck)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Concurrent.STM (TVar, atomically, readTVar, modifyTVar')
import Servant.Server.Internal.DelayedIO (withRequest, delayedFailFatal)
import Data.Time.TypeLevel (TimePeriod,  KnownDuration, DurationUnit, durationMicroseconds)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime, nominalDiffTimeToSeconds)

import Text (textToInt)


data State = State { timeoutMap :: Map Int64 UTCTime, allowedUsers :: [Int64] }


-- The state now stores the time when the next request is allowed for each IP.
type RateLimitState = TVar State

data RateLimitPerUser (delay :: TimePeriod) (user :: Maybe Symbol)

-- This class is the bridge from the type-level 'Maybe Nat' to the
-- value-level 'Maybe Integer'.
class HasRateLimitUser (user :: Maybe Symbol) where
  getUser :: Proxy user -> Maybe Text

-- The instance for the 'Nothing case. If the type is 'Nothing, the value is Nothing.
instance HasRateLimitUser 'Nothing where
  getUser _ = Nothing

-- The instance for the 'Just n case.
-- We require 'KnownNat n' to get the value of 'n'.
instance KnownSymbol n => HasRateLimitUser ('Just n) where
  getUser _ = Just (T.pack (symbolVal (Proxy @n)))


-- We can reuse RateLimitExceeded, or create a more specific one.
-- Let's create a more descriptive error type.
data RateLimitException = MalformedUserId { reason :: Text } deriving (Show, Exception)

-- A custom exception to carry our "time left" value.
-- data RateLimitExceeded =
--      RateLimitExceeded
--      { timeLeftSeconds :: Integer -- The number of seconds the user must wait.
--      } deriving (Show)

-- We'll create a JSON response for our error.
data RateLimitError =
     RateLimitError
     { error   :: Text
     , retryIn :: Text
     } deriving (Show, Generic)

instance ToJSON RateLimitError

-- New signature! No more exceptions from this function.
checkRateLimit :: NominalDiffTime -> Maybe Text -> RateLimitState -> Request -> IO (Maybe Integer)
checkRateLimit _ Nothing _ _ = pure Nothing 
checkRateLimit delay (Just userParamName) stateTVar req = do
  now <- getCurrentTime
  let queryParams = queryToQueryText $ queryString req
  let maybeUserId = lookup userParamName queryParams

  case maybeUserId of
    Nothing -> return Nothing -- SUCCESS
    Just Nothing -> throwIO $ MalformedUserId $ "Required query parameter '" <> userParamName <> "' is missing."
    Just (Just userIdText) -> do
      let maybeUserId = fmap fromIntegral $ textToInt userIdText
      case maybeUserId of 
        Nothing -> throwIO $ MalformedUserId $ "Query parameter '" <> userParamName <> "' must be an integer."
        Just userId ->
          atomically $ do
            state <- readTVar stateTVar
            if userId `elem` allowedUsers state then 
              return Nothing
            else do
              let stateMap = timeoutMap state
              case Map.lookup userId stateMap of
                -- Case 1: user is in the map.
                Just nextAllowedTime ->
                  if now >= nextAllowedTime then do
                    -- The user waited long enough. Allow and update the timestamp.
                    let newNextTime = addUTCTime delay now
                    let newTimeoutMap = Map.insert userId newNextTime stateMap
                    fmap (const Nothing) $ 
                      modifyTVar' stateTVar $ \s -> 
                        s { timeoutMap = newTimeoutMap } -- SUCCESS
                  else do
                    -- The user is too early. Deny and calculate the wait time.
                    let timeLeft = ceiling (nextAllowedTime `diffUTCTime` now)
                    pure (Just timeLeft) -- FAILURE
                -- Case 2: user is not in the map (first request).
                Nothing -> do
                  let newNextTime = addUTCTime delay now
                  let newTimeoutMap = Map.insert userId newNextTime stateMap
                  fmap (const Nothing) $ 
                    modifyTVar' stateTVar $ \s -> 
                      s { timeoutMap = newTimeoutMap } -- SUCCESS


-- Our final HasServer instance, based on the library's source code.
instance ( HasServer api context
         , HasContextEntry context RateLimitState
         , KnownDuration delay
         , HasRateLimitUser user
         , TimeUnit (DurationUnit delay)
         ) => HasServer (RateLimitPerUser delay user :> api) context where

  type ServerT (RateLimitPerUser delay user :> api) m = ServerT api m

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
        
        -- user id if present
        maybeUser :: Maybe Text
        maybeUser = getUser (Proxy @user)

        rateLimitState = getContextEntry context

        handleErrors (MalformedUserId reason) = pure $ Left reason

        -- Define the check that will be run on each request.
        -- This uses 'withRequest' to gain access to the 'Request' object.
        rateCheck = withRequest $ \req -> do
          -- Apply our rate-limiting logic.
          eitherSeconds <- liftIO $ handle @RateLimitException handleErrors $ fmap Right $ checkRateLimit rateLimitDelay maybeUser rateLimitState req
          
          -- Handle the result.
          case eitherSeconds of
            -- SUCCESS: The request is allowed. Do nothing and let it proceed.
            Right Nothing -> pure ()
            -- FAILURE: The rate limit was exceeded.
            Right (Just seconds) ->
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
            Left err -> delayedFailFatal $ err400 { errBody = encode err }
    in
      -- Attach the check to the server and route it.
      -- Note: The library uses a helper `addAcceptCheck`. This function is
      -- not exported, but we can achieve the same result by applying our
      -- check monadically before running the subserver.
      route (Proxy :: Proxy api) context (addAcceptCheck subserver rateCheck)