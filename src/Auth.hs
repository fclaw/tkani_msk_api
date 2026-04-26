{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

module Auth (verifyAdmin, AdminUser, HashedAdminPassword (..)) where

import           Data.Password.Bcrypt
import           Data.Text           (Text, pack)
import           Data.Text.Encoding  (decodeUtf8)
import           Servant             (BasicAuthData(..))
import           Control.Lens        ((^.))
import           Servant.Server      (BasicAuthCheck(..), BasicAuthResult(..))

import           App                 (Config, adminUser, adminPassHash) -- Your env getters

-- | 1. The Resulting Identity
-- | If Auth succeeds, this object is passed into your handler.
newtype AdminUser = AdminUser { adminName :: Text } 
  deriving (Show, Eq)

-- | 2. The Verification Logic
-- | It takes your App Environment so it can compare the 
-- | incoming password with your Config.
verifyAdmin :: Config -> BasicAuthCheck AdminUser
verifyAdmin cfg = BasicAuthCheck $ \creds -> 
  let
    -- Values from your bot config/env variables
    validUser    = cfg ^. adminUser
    passwordHash = cfg ^. adminPassHash
    
    -- Values from the 'Authorization' header sent by client/curl
    incomingUser = decodeUtf8 (basicAuthUsername creds)
    incomingPass = mkPassword $ decodeUtf8 (basicAuthPassword creds)
    passwordCheck = checkPassword incomingPass passwordHash
  in
    if incomingUser == validUser && 
       passwordCheck == PasswordCheckSuccess
      then return (Authorized $ AdminUser incomingUser)
    else return BadPassword


newtype HashedAdminPassword = HashedAdminPassword 
  { getBcryptHash :: PasswordHash Bcrypt 
  } deriving (Show)

instance Read HashedAdminPassword where
  readsPrec _ s = 
    -- Clean the string if it comes with surrounding quotes
    let clean = filter (/= '"') s
    in [(HashedAdminPassword (PasswordHash (pack clean)), "")]