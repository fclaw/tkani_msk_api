-- src/TH/RecordToTuple.hs
{-# LANGUAGE TemplateHaskell #-}

module TH.RecordToTuple where

import Language.Haskell.TH

recordToTuple :: Name -> Q Exp
recordToTuple name = do
  -- 1. Reify the initial name. As you discovered, this might be a DataConI.
  info <- reify name

  parentTypeName <- case info of
    -- --- THE FIX (Your Correct Pattern) ---
    -- If we get a Data Constructor, we extract its parent's type name.
    DataConI _ _ parentTypeName   -> pure parentTypeName -- Newer GHC (let's handle both)

    -- If we get a Type Constructor directly, we use its name.
    TyConI (DataD _ parentTypeName _ _ _ _) -> pure parentTypeName
    
    _ -> fail $ "recordToTuple: Expected a data or type constructor, but got: " ++ show info

  -- 2. Now, reify the PARENT TYPE to get the field names.
  --    This is guaranteed to give us a TyConI.
  parentInfo <- reify parentTypeName
  
  fields <- case parentInfo of
    TyConI (DataD _ _ _ _ [RecC _ fieldTypes] _) -> pure fieldTypes
    _ -> fail $ "recordToTuple: The parent type is not a single-constructor record: " ++ show parentInfo
  
  -- 3. The rest of the function is the same as before.
  recordVarName <- newName "record"
  
  let fieldAccessors = [ VarE fieldName | (fieldName, _, _) <- fields ]
  let fieldExpressions = [ accessor `AppE` VarE recordVarName | accessor <- fieldAccessors ]
        
  let tupleBody = TupE (map Just fieldExpressions)
  
  return $ LamE [VarP recordVarName] tupleBody