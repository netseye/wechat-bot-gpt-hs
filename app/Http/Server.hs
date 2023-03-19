{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Http.Server where

import Control.Monad.IO.Class (liftIO)
import Http.Bot (callBack)
import Http.Types (BotReq, Message, Resp)
-- ------------------------------------------------

import Network.Wai.Handler.Warp (run)
import Servant
  ( Capture,
    Get,
    Handler,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.UI
  ( SwaggerSchemaUI,
    swaggerSchemaUIServer,
  )

http :: IO ()
http = do
  putStrLn "访问 http://localhost:7700/swagger-ui/ 查看文档"
  run 7700 $ serve (Proxy :: Proxy APIWithSwagger) serverAPIWithSwagger

type API =
  Get '[JSON] (String, String)
    :<|> "callback" :> Capture "name" String :> ReqBody '[JSON] BotReq :> Post '[JSON] (Resp Message)

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