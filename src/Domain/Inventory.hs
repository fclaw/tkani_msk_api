module Domain.Inventory (adjustInventoryForOrder, InventoryResult(..)) where


import Data.Text (Text)

import App (AppM)


data InventoryResult 
  = StockOK 
  | FabricSoldOut FabricInfo -- We pass info back to create a nice message
  deriving (Show)

data FabricInfo = FabricInfo { fiName :: Text, fiArticle :: Text } deriving (Show)

-- Threshold: If stock falls below this, hide the fabric.
thresholdMetres :: Double
thresholdMetres = 1.0

adjustInventoryForOrder :: Text -> AppM (Either Text InventoryResult)
adjustInventoryForOrder _ = undefined