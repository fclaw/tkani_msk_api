{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.PdfGenerator (generateConsignmentPdf) where

import           Data.Aeson                  (ToJSON, toJSON)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T
import           Data.Text.Lazy              (toStrict)
import           GHC.Generics                (Generic)
import           System.Process.Typed
import           System.Exit                 (ExitCode(..))
import           Text.Ginger                 (easyRender, parseGingerFile)  
import           Data.Aeson                  (object, (.=))
import           Control.Monad.IO.Class      (liftIO)
import           System.Directory            (doesFileExist)
import qualified Data.Text.Encoding          as T
import qualified Data.Text.IO                as TIO

import           App                         (AppM)
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
        
      -- 2. Use Aeson's object builder instead of a custom function.
      -- This is automatically compatible with easyRender.
      let context = object 
            [ "orderId"  .= orderId
            , "items"    .= items
            , "totalSum" .= (total :: Int)
            ]

      -- 3. Render Template to HTML (Text)
      let htmlContent = easyRender context template
      -- Render htmlContent to ByteString (UTF-8)
      let htmlInput = BL.fromStrict $ T.encodeUtf8 htmlContent
        
      -- FIX: Use readProcessWithExitCode for binary data
      liftIO $ do
        -- setStdin (byteStringInput ...) and setStdout byteStringOutput
        -- ensures binary data is handled correctly.
        let p = setStdin (byteStringInput htmlInput)
                $ setStdout byteStringOutput
                $ setStderr byteStringOutput
                $ proc "/usr/local/bin/wkhtmltopdf" ["--quiet", "-", "-"]

        (exitCode, pdfStdout, stderr) <- readProcess p
    
        case exitCode of
          ExitSuccess -> 
            -- pdfStdout is already a strict ByteString, no decoding needed!
            pure $ Right $ BL.toStrict pdfStdout
          ExitFailure n -> do
            -- stderr is binary too, decode it to see what went wrong
            let errDetail = T.decodeUtf8  $ BL.toStrict stderr
            pure $ Left $ "wkhtmltopdf failed (exit " <> T.pack (show n) <> "): " <> errDetail