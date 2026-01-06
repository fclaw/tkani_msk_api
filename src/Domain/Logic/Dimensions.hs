{-|
    __Important Considerations & Future Improvements__

    This estimation model uses a set of heuristics ("magic numbers") that provide a strong
    starting point but should be refined over time with real-world data.

    1.  **Configuration over Hardcoding:**
        The values for `thicknessCm` based on density, the standard `foldedLengthCm`,
        `rollingThresholdMeters`, and `paddingCm` are currently hardcoded. For better
        maintainability, these should be moved to a dedicated configuration record
        (e.g., `DimensionsConfig`) and loaded from `config.yaml`. This allows for
        tweaking the model without recompiling the application.

    2.  **Fabric "Stiffness" Factor:**
        The current model calculates dimensions based purely on volume. However, the
        "stiffness" or "pliability" of a fabric significantly affects its packed shape.
        A future, more advanced model could introduce a "stiffness factor" for
        different fabric types (e.g., Denim, Silk, Wool). For example, a stiff denim
        will form a much thicker and less compact roll than a soft silk of the
        exact same volume.

    3.  **Real-World Data Feedback Loop:**
        The best way to improve this model is to collect data. Periodically comparing the
        estimated dimensions with the actual, physically measured dimensions of packed
        orders will allow for fine-tuning the thickness and padding constants,
        leading to more accurate shipping quotes.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Domain.Logic.Dimensions 
       ( FabricDensity (..)
       , estimatePackedDimensions
       , estimatePackedWeight
       , packagingWeightGrams
       ) where


import Data.Aeson
import Data.Aeson.TH
import GHC.Generics (Generic)

import Text (camelToSnake)


data FabricDensity = Light | SemiLight | Dense
  deriving (Show, Eq, Generic)

$(deriveJSON 
    defaultOptions 
    { constructorTagModifier = camelToSnake
    , sumEncoding = UntaggedValue } 
    ''FabricDensity)

-- Define your standard folded package base dimensions (in cm)
foldedLengthCm, foldedWidthCm :: Double
foldedLengthCm = 40.0
foldedWidthCm = 30.0

-- Define the threshold for switching from folding to rolling (in meters)
rollingThresholdMeters :: Double
rollingThresholdMeters = 6.0

-- | Estimates the packed dimensions based on whether the fabric is folded or rolled.
--   All inputs and outputs are in centimeters.
estimatePackedDimensions
    :: FabricDensity -- ^ The density category of the fabric
    -> Double        -- ^ The working width of the roll (cm)
    -> Double        -- ^ The length to be cut (meters)
    -> (Int, Int, Int) -- ^ Returns a tuple of (Length, Width, Height) in cm
estimatePackedDimensions density workingWidth cutLengthM
    -- THE BRANCHING LOGIC:
    -- If the cut is long, we roll it. Otherwise, we fold it.
    | cutLengthM >= rollingThresholdMeters = calculateRolledDimensions
    | otherwise                            = calculateFoldedDimensions
  where
    -- First, calculate the common variables
    thicknessCm = case density of
        Light     -> 0.02
        SemiLight -> 0.05
        Dense     -> 0.12
    volumeCm3 = (cutLengthM * 100) * workingWidth * thicknessCm
    paddingCm = 5.0 -- General padding for the box/bag

    -- === LOGIC 1: Calculate for a FOLDED stack ===
    calculateFoldedDimensions :: (Int, Int, Int)
    calculateFoldedDimensions =
        let
            baseAreaCm2 = foldedLengthCm * foldedWidthCm
            stackHeightCm = if baseAreaCm2 > 0 then volumeCm3 / baseAreaCm2 else 0

            finalLength = ceiling (foldedLengthCm + paddingCm)
            finalWidth  = ceiling (foldedWidthCm + paddingCm)
            finalHeight = ceiling (stackHeightCm + paddingCm)
        in
            (finalLength, finalWidth, max 1 finalHeight)

    -- === LOGIC 2: Calculate for a ROLLED cylinder ===
    calculateRolledDimensions :: (Int, Int, Int)
    calculateRolledDimensions =
        let
            -- The length of the package is determined by the fabric width (folded in half)
            finalLength = ceiling ((workingWidth / 2.0) + paddingCm)
            
            -- The cross-section determines the width and height
            crossSectionalArea = if finalLength > 0
                                 then volumeCm3 / fromIntegral finalLength
                                 else 0
            
            sideLength = sqrt crossSectionalArea -- Assuming a square-ish roll
            
            finalWidth  = ceiling (sideLength + paddingCm)
            finalHeight = ceiling (sideLength + paddingCm)
        in
            (finalLength, finalWidth, finalHeight)



-- Example Usage
-- A 2.5m cut of semi-light fabric with a 140cm width
-- let (l, w, h) = estimatePackedDimensions SemiLight 140.0 2.5
-- l -> 43
-- w -> 33
-- h -> 4


-- Standard weight of packaging materials in grams.
-- This should ideally come from your application's configuration.
packagingWeightGrams :: Int
packagingWeightGrams = 150

-- | Estimates the total shipping weight of a fabric order in grams.
--
--   Args:
--     weightPerMeterGrams: The weight of the fabric in grams per meter.
--     cutLengthM: The length of the fabric cut in meters.
--
--   Returns:
--     The total estimated weight of the parcel in grams (Int).
estimatePackedWeight
    :: Int
    -> Double -- ^ Weight per meter (grams)
    -> Double -- ^ Length to cut (meters)
    -> Int    -- ^ Returns total weight in grams
estimatePackedWeight packagingWeightGrams weightPerMeterGrams cutLengthM =
    let
        -- Step 1: Calculate the weight of the fabric itself
        fabricWeight = weightPerMeterGrams * cutLengthM
        
        -- Step 2: Add the fixed packaging weight
        totalWeight = fabricWeight + fromIntegral packagingWeightGrams

    -- Round up to the nearest whole gram for the API
    in ceiling totalWeight

-- Example Usage
-- let totalWeight = estimatePackedWeight 350.0 2.5
-- totalWeight will be 1026 (875.0 + 150 = 1025.0, ceiling -> 1026 to be safe, or just round)
-- A 'round' might be better if you prefer standard rounding.
-- let totalWeight = round (fabricWeight + fromIntegral packagingWeightGrams) -> 1025