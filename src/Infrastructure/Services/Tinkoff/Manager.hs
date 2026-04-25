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
  eitherCred <- TLS.credentialLoadX509 certPath keyPath
  case eitherCred of
    Left err -> error $ "Failed to load T-Bank credentials: " <> err
    Right creds -> do
      -- 2. Configure the TLS Client parameters specifically for the Tinkoff host
      let baseParams = TLS.defaultParamsClient hostName mempty
      let clientParams = 
            baseParams
            { TLS.clientHooks = TLS.defaultClientHooks
                -- Provide the certificate specifically when requested
               { TLS.onCertificateRequest = \_ -> return (Just creds)
               , TLS.onServerCertificate = \_ _ _ _ -> return []  
               }
            , TLS.clientShared = TLS.defaultShared
                -- IMPORTANT: We also add it to the shared credentials list 
                -- to help the proxy/tunnel negotiate the cert
               { TLS.sharedCredentials = TLS.Credentials [creds] }
            , TLS.clientSupported = TLS.defaultSupported
               { TLS.supportedCiphers = TLS.ciphersuite_all }
            }
      -- 3. Create and return the Manager using mkManagerSettings from http-client-tls
      newManager $ mkManagerSettings (TLSSettings clientParams) Nothing