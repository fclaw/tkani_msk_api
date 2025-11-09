{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Data.Text (Text)

-- 1. Define the API type using Servant's type-level DSL.
--    This defines a single endpoint: GET "/" which returns plain text.
type API = Get '[PlainText] Text

-- 2. Create a 'Proxy' for our API type.
--    Servant uses this to work with the API type at the value level.
api :: Proxy API
api = Proxy

-- 3. Implement the handler for our API.
--    The type signature of the handler must match the API type.
--    Here, it's a simple 'Handler' that returns a 'Text'.
server :: Server API
server = return "Hello, World! This is the Servant/Warp test server."

-- 4. Create the WAI Application.
--    'serve' turns our Servant API and its handlers into a standard WAI Application.
app :: Application
app = serve api server

-- 5. The main entry point.
--    It starts the Warp server on port 8080 and tells it to run our 'app'.
main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Starting server on http://localhost:" ++ show port
  run port app