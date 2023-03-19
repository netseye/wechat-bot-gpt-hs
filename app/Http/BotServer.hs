{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Http.BotServer where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import Database.Redis
  ( connect,
    defaultConnectInfo,
    publish,
    runRedis,
  )
import Http.Error
import Http.Types
  ( BotReq,
    Info (Info, text),
    Message (..),
    Resp (..),
  )
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant
  ( Capture,
    Context (EmptyContext, (:.)),
    JSON,
    Post,
    Proxy (..),
    Server,
    serveWithContext,
    type (:>),
  )
import Servant.Multipart (Mem, MultipartForm)

type BotAPI = "callback" :> Capture "name" String :> MultipartForm Mem BotReq :> Post '[JSON] (Resp Message)

api :: Proxy BotAPI
api = Proxy

callback :: Server BotAPI
callback name req = do
  liftIO $ do
    putStrLn name
    print req
    conn <- connect defaultConnectInfo
    x <- runRedis conn $ publish "msg" (BSL.toStrict $ encode req)
    case x of
      Left err -> print err
      Right _ -> putStrLn "task send success"
  return
    Resp
      { message = "success",
        code = 0,
        respData =
          Message
            { messageType = 5000,
              info =
                Info
                  { text = ""
                  }
            }
      }

startServer :: IO ()
startServer = do
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 7700 $ setLogger aplogger defaultSettings
    runSettings
      settings
      ( serveWithContext api (customFormatters :. EmptyContext) callback
      )