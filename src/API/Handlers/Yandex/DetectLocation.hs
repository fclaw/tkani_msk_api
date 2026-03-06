{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.Yandex.DetectLocation (handler) where

import qualified Data.Text as T
import Katip (logTM, Severity(..), ls)
import Data.Text (Text)
import Data.Functor ((<&>))
import Data.Foldable (foldl')

import App (AppM)
import Text (tshow)
import TH.Location (currentModule)
import API.Types (ApiResponse, mkError, YandexDeliveryCity (..))
import Infrastructure.Services.Yandex.Types (LocationDetectReq (..), LocationDetectedVariant (..))
import Infrastructure.Services.Yandex (detectLocation)



-- | A dictionary of common Russian administrative abbreviations
abbreviateRegion :: T.Text -> T.Text
abbreviateRegion = T.strip . applyReplacements
  where
    applyReplacements text = foldl' (\t (old, new) -> T.replace old new t) text rules
    
    rules = 
      [ -- 1. Longest compound phrases (Process these FIRST)
        ("посёлок городского типа", "пгт")
      , ("п. городского типа", "пгт")
      , ("городской округ", "г.о.")
      , ("муниципальный округ", "м.о.")
      , ("автономный округ", "АО")
      
      -- 2. Settlements and Entities (Note: spaces are included for safety)
      , ("посёлок ", "п. ")
      , ("деревня ", "д. ")
      , ("село ", "с. ")
      , ("станица ", "ст-ца ")
      
      -- 3. Regional / Political entities
      , ("область", "обл.")
      , ("край", "кр.")
      , ("Республика", "Респ.")
      , ("республика", "респ.")
      , ("город ", "г. ")  -- Trailing space prevents breaking names like "Белгород"
      , ("район", "р-н")
      ]

-- | Universal formatter for any Russian delivery API response
renderUniversalTgLabel :: T.Text -> T.Text
renderUniversalTgLabel addr =
    let parts = T.splitOn ", " addr
    in case parts of
        -- 1. Just a major city
        [single] -> "🏙 " <> abbreviateRegion single
        
        -- 2. Multi-part address
        (identity:rest) | not (null rest) -> 
            let -- We simplify BOTH the identity and the region
                cleanIdent  = abbreviateRegion identity
                cleanRegion = abbreviateRegion (last rest)
            in "📍 " <> cleanIdent <> " (" <> cleanRegion <> ")"
            
        -- 3. Fallback (though usually case 1 or 2 cover everything)
        (identity:_) -> "📍 " <> abbreviateRegion identity
        []           -> "📍 ..."


handler :: Maybe Text -> AppM (ApiResponse [YandexDeliveryCity])
handler Nothing = pure $ Right []
handler (Just location) = do 
  eRes <- detectLocation (LocationDetectReq ("Россия, " <> location))
  case eRes of
    Left err -> 
      fmap (const (Left (mkError "server error"))) $ 
        $(logTM) ErrorS $ "Failed to detect location: " <> ls (tshow err)
    Right res -> pure $ Right $ res <&> \variant -> 
      YandexDeliveryCity (geoId variant) (renderUniversalTgLabel (address variant))
