{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Infrastructure.Services.Tinkoff.Manager (setupManager) where

import           System.FilePath          (FilePath)
import           Network.HTTP.Client      (Manager, newManager)
import           Network.HTTP.Client.TLS  (mkManagerSettings)
import           Network.Connection       (TLSSettings (..))
import qualified Network.TLS              as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import           Control.Exception        (throwIO)
import           Data.Default.Class       (def)

-- | Sets up the specialized mTLS Manager for Tinkoff Open API calls.
-- | Requires paths to your .pem and .key files.
setupManager :: TLS.HostName -> FilePath -> FilePath -> IO Manager
setupManager hostName certPath keyPath = do
    -- 1. Load the Certificate and Private Key from PEM files
    -- This mimics the --cert and --key flags in curl
    cred <- TLS.credentialLoadX509 certPath keyPath >>= \case
      Right c -> return c
      Left err -> error $ "Failed to load T-Bank credentials: " <> err

    -- 2. Configure the TLS Client parameters specifically for the Tinkoff host
    let base = TLS.defaultParamsClient hostName mempty
    let clientParams = base
         { TLS.clientHooks = (TLS.clientHooks base)
           { TLS.onCertificateRequest = 
             const $ return (Just cred) }
         , TLS.clientSupported = def 
           { TLS.supportedCiphers = 
             TLS.ciphersuite_all }
         }

    -- 3. Create and return the Manager using mkManagerSettings from http-client-tls
    newManager $ mkManagerSettings (TLSSettings clientParams) Nothing