-- file: src/Infrastructure/Templating.hs
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Templating
  ( TemplateMap
  , TemplateData
  , loadTemplatesFromDirectory
  , renderTemplate
  ) where

import           Control.Monad          (filterM, forM)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import           System.Directory       (doesDirectoryExist, doesFileExist,
                                         listDirectory)
import           System.FilePath        ((</>), (<.>), normalise,
                                         pathSeparator, takeBaseName,
                                         stripExtension, splitExtension)
import Data.Function ((&))


-- | A map from a module-like path (e.g., "Handlers.PlaceNewOrder") to its template content.
type TemplateMap = HashMap Text Text

-- | A simple type alias for our template data map.
type TemplateData = HashMap Text Text

-- | Renders a template by replacing placeholders with data from a map.
--   Placeholders are in the format "{{key}}".
renderTemplate :: Text -> TemplateData -> Text
renderTemplate template dataMap =
  -- We fold over the key-value pairs in the map, and for each one,
  -- we perform a replacement on the template text.
  HM.foldlWithKey' applyReplacement template dataMap
  where
    applyReplacement :: Text -> Text -> Text -> Text
    applyReplacement currentText key value =
      let placeholder = "{{" <> key <> "}}"
      in T.replace placeholder value currentText


-- | Recursively scans a directory for files with a given extension (e.g., ".tpl")
--   and loads them into a map. The keys are formatted to look like module names.
loadTemplatesFromDirectory :: FilePath -> IO TemplateMap
loadTemplatesFromDirectory rootDir = do
  -- First, get all files ending in ".tpl" recursively
  tplFiles <- findFilesByExt ".tpl" rootDir
  
  -- Create a list of key-value pairs (module-like path, file content)
  pairs <- forM tplFiles $ \filePath -> do
    content <- TIO.readFile (rootDir </> filePath)
    let moduleKey = T.pack $ convertPathToModule filePath
    pure (moduleKey, content)
  pure $ HM.fromList pairs

--
-- -- Helper functions for file traversal --
--
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topPath = do
  names <- listDirectory topPath
  paths <- forM names $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else pure [path]
  pure (concat paths)

findFilesByExt :: String -> FilePath -> IO [FilePath]
findFilesByExt ext dir = do
  allFiles <- getRecursiveContents dir
  let projectRootLen = length (normalise (dir ++ [pathSeparator]))
  -- Filter by extension and make paths relative to the root
  pure $ filter (\f -> ext == snd (splitExtension f))
       $ map (drop projectRootLen) allFiles

-- Converts "Handlers/PlaceNew-Order.tpl" to "Handlers.PlaceNew-Order"
-- And "Handlers\PlaceNew-Order.tpl" on Windows
convertPathToModule :: FilePath -> String
convertPathToModule = map fixSeparator . dropExtension
  where
    fixSeparator c = if isPathSeparator c then '.' else c

-- 'splitExtension' and 'dropExtension' don't work with multi-dots
-- this is a simpler replacement for just dropping '.tpl'
dropExtension :: String -> String
dropExtension path = T.init (fst (T.breakOnEnd "." (T.pack path))) & T.unpack

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'