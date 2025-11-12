-- We need TemplateHaskell to generate functions from SQL files
{-# LANGUAGE TemplateHaskell       #-}
-- Also helpful for writing the TH splices
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}

module DB
  ( getFabricInfoById
  , putNewFabric
  ) where


import qualified Hasql.Pool as Hasql
import qualified Hasql.Transaction as Hasql
import qualified Hasql.Transaction.Sessions as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.TH as TH
import Data.Profunctor.Unsafe (dimap, lmap)
import Data.Aeson (FromJSON, fromJSON, Result (..), Value)
import Data.Text (Text, pack)
import Data.Bifunctor (first, second)
import Control.Monad (join)
import Data.Tuple.Ops (initT, app2, app3)


import API.Types (FabricInfo(..), PreCut(..), FullFabric) -- Your data types
import TH.RecordToTuple (recordToTuple)
import API.WithField (WithField)


--------------------------------------------------------------------------------
-- Template Haskell Magic: Generate our statement functions automatically
--------------------------------------------------------------------------------

convertFromJson :: FromJSON a => Value -> Either String a
convertFromJson value =
  case fromJSON value of
   Success fabricInfo -> Right fabricInfo
   Error msg -> Left msg


runTransaction pool mode = Hasql.use pool . Hasql.transaction Hasql.Serializable mode

-- | Statement to fetch a single fabric row by its ID.
--   TH.singletonStatement reads the SQL, infers the parameter and result types.
getFabricStatement :: Hasql.Statement Int (Either String FullFabric)
getFabricStatement =
  dimap fromIntegral (maybe (Left "fabric not found") id . fmap (convertFromJson @FullFabric))
  [TH.maybeStatement|
    select json_build_object(
      'fiId', f.id,
      'fiDescription', f.description,
      'fiTotalLengthM', f.total_length_m,
      'fiPricePerMeter', f.price_per_meter,
      'fiAvailableLengthM', f.available_length_m,
      'fiIsSold', f.is_sold,
      'fiPreCuts', coalesce((
        select jsonb_agg(
          json_build_object(
          'pcId', pc.id,
          'pcLengthM', pc.length_m,
          'pcPriceRub', pc.price_rub,
          'pcInStock', pc.in_stock)
         ) :: jsonb
         from pre_cuts AS pc
         where pc.fabric_id = f.id and pc.in_stock = TRUE),
         '[]'::jsonb)) :: jsonb
    from fabrics AS f
    where f.id = $1 :: int4
  |]


-- | Fetches a fabric and all its associated, in-stock pre-cuts from the database.
getFabricInfoById :: Int -> Hasql.Pool -> IO (Either Text FullFabric)
getFabricInfoById fabricId_ pool = fmap (join . first (pack . show)) $ runTransaction pool Hasql.Read $ fmap (first pack) $ fabricId_ `Hasql.statement` getFabricStatement

putNewFabricStatement :: Hasql.Statement FabricInfo Int
putNewFabricStatement = 
  dimap (app2 fromIntegral . app3 fromIntegral . initT . $(recordToTuple 'FabricInfo)) fromIntegral
  [TH.singletonStatement|
    insert into fabrics (description, total_length_m, price_per_meter, available_length_m)
    values ($1 :: text, $2 :: int4, $3 :: int4, $4 :: float8)
    returning id :: int4
  |]

putNewFabric :: FabricInfo -> Hasql.Pool -> IO (Either Text Int)
putNewFabric fabricInfo_ pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Write $ fabricInfo_ `Hasql.statement` putNewFabricStatement