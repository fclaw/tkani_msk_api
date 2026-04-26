{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Services.PdfGenerator (generateConsignmentPdf) where

import           Data.Aeson                  (ToJSON, toJSON)
import qualified Data.ByteString             as B
import qualified Data.Text                   as T
import           GHC.Generics                (Generic)
import           System.Exit                 (ExitCode(..))
import           Text.Ginger                 (easyRender, parseGingerFile)  
import           Data.Aeson                  (object, (.=))
import           Control.Monad.IO.Class      (liftIO)
import           System.Directory            (doesFileExist)
import qualified Data.Text.Encoding          as T
import qualified Data.Text.IO                as TIO
import           Network.Wreq                ( postWith, defaults, auth, basicAuth
                                             , partBS, responseBody, partText
                                             , responseStatus, statusCode
                                             )
import           Control.Exception           (try)
import           Network.HTTP.Client         (HttpException)
import           Control.Lens                ((&), (?~), (^.))
import           Control.Monad.Reader.Class  (ask)
import           Data.Time                   (getCurrentTime, utctDay, showGregorian)

import           App                         (AppM, pdfServiceUrl)
import           Infrastructure.Database     (ConsignmentPdfItem (..))



-- | A helper to resolve template paths from the disk
-- | This is the missing piece that tells Ginger how to "find" the file
fileResolver :: FilePath -> IO (Maybe T.Text)
fileResolver path = do
  exists <- doesFileExist path
  if exists 
    then Just <$> TIO.readFile path 
    else return Nothing


generateConsignmentPdf :: T.Text -> [ConsignmentPdfItem] -> AppM (Either T.Text B.ByteString)
generateConsignmentPdf orderId items = do
  -- 1. Load the Ginger template (ginger supports includes/imports)
  -- 'Nothing' for the second param uses the default file resolver
  mTemplate <- liftIO $ parseGingerFile (fmap (fmap T.unpack) . fileResolver) "assets/templates/consignment_note.html"
  
  case mTemplate of
    Left err -> pure $ Left $ "Template parse error: " <> T.pack (show err)
    Right template -> do
      let total = sum $ map cpdfTotalPrice items
        
      now <- liftIO getCurrentTime
      let timestamp = T.pack $ showGregorian (utctDay now)

      -- 2. Use Aeson's object builder instead of a custom function.
      -- This is automatically compatible with easyRender.
      let context = object 
            [ "orderId"   .= orderId
            , "items"     .= items
            , "totalSum"  .= (total :: Int)
            , "timestamp" .= timestamp    -- NEW: Today's date
            ]

      -- 3. Render Template to HTML (Text)
      let htmlContent = easyRender context template

      cfg <- ask
  
      let url = T.unpack (cfg ^. pdfServiceUrl) <> "/convert"

      -- Prepare the Auth options
      let opts = defaults
    
      let payload = 
            [ -- 1. THE SOURCE DATA
              -- We use "text" instead of "file" to send raw HTML string from RAM
              partText "text"                   htmlContent
            
              -- 2. INPUT FORMATTING
            , partText "input_format"           "html"
            
              -- 3. LAYOUT CONTROL
              -- Ensures the table fits width-wise on the A4 page
            , partText "content_viewport_width" "balanced"
            
              -- 4. OUTPUT OPTIONS (Professional Additions)
            , partText "page_size"              "A4"
            , partText "orientation"            "portrait"
            ]

      -- 4. Execute the request
      liftIO (try $ postWith opts url payload) >>= \case
        Left (err :: HttpException) -> 
          pure $ Left $ "Local PDF service failed: " <> T.pack (show err)
        
        Right resp ->
          let code = resp ^. responseStatus . statusCode
          in if code == 200
             then pure $ Right (B.toStrict $ resp ^. responseBody)
              else pure $ Left $ "PDF Service Error: Status " <> T.pack (show code)