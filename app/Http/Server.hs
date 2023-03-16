
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}

module Http.Server where

import Control.Monad.IO.Class (liftIO)
import Http.Bot
import Http.Types
-- ------------------------------------------------

import Network.Wai.Handler.Warp
import Servant
import Servant.Swagger
import Servant.Swagger.UI

http :: IO ()
http = do
  putStrLn "访问 http://localhost:7700/swagger-ui/ 查看文档"
  run 7700 $ serve (Proxy :: Proxy APIWithSwagger) serverAPIWithSwagger

type API =
  Get '[JSON] (String, String)
    :<|> "callback" :> Capture "name" String :> ReqBody '[JSON] Message :> Post '[JSON] (Resp Message)

type APIWithSwagger =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> API

status :: Handler (String, String)
status = do
  liftIO $ putStrLn "status"
  pure ("200", "success")

serverAPI :: Server API
serverAPI = status :<|> callBack 

serverAPIWithSwagger :: Server APIWithSwagger
serverAPIWithSwagger = swaggerSchemaUIServer (toSwagger (Proxy :: Proxy API)) :<|> serverAPI