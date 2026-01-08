-- src/TH/RecordToTuple.hs
{-# LANGUAGE TemplateHaskell #-}

module TH.RecordToTuple (recordToTuple, tupleToRecord) where

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



-- | A Template Haskell function that generates a function to convert a tuple
--   to a record of the given type.
--
--   Usage: $(mkFromTuple ''MyRecordType)
--
tupleToRecord :: Name -> Q Exp
tupleToRecord typeName = do
    -- 1. Get information about the record type (its name, constructors, fields)
    TyConI (DataD _ _ _ _ [RecC conName fields] _) <- reify typeName

    -- 2. Create variable names for the tuple and its elements
    tupleName <- newName "tpl"
    fieldNames <- mapM (newName . nameBase . fst') fields

    -- 3. Build the function body
    --    This will generate code that looks like:
    --    \tpl -> let (f1, f2, ...) = tpl in MyRecordType { field1 = f1, field2 = f2, ... }
    let body = LamE [VarP tupleName] $ -- \tpl ->
                 LetE [ValD (TupP (map VarP fieldNames)) (NormalB (VarE tupleName)) []] $ -- let (f1, f2) = tpl in
                     RecConE conName $ -- MyRecordType { ... }
                         zipWith (\(fieldName, _, _) varName -> (fieldName, VarE varName)) fields fieldNames

    return body
  where
    fst' (a, _, _) = a