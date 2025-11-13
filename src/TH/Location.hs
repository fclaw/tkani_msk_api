module TH.Location (currentModule) where

import Language.Haskell.TH (loc_module, location, Loc(..), Q, Exp, stringE)
import Data.Text (Text, pack)

-- This must be in a 'do' block within a Monad.
-- `location` is a TH function that returns the current Loc.
-- `loc_module` is the field accessor for the module name in the Loc record.
-- The `lift` is needed because `location` returns a value of type `Q Loc`.
currentModule :: Q Exp -- 'Q Exp' is a Template Haskell expression
currentModule = stringE . loc_module =<< location