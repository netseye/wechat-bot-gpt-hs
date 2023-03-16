{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Http.BotServer where

import Control.Monad
import Control.Monad.IO.Class
import Http.Types
import Network.Wai.Handler.Warp
import Servant
import Servant.Multipart

type API = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Resp Message)

api :: Proxy API
api = Proxy

-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
callback :: Server API
callback multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $
        "  "
          ++ show (iName input)
          ++ " -> "
          ++ show (iValue input)
  return
    Resp
      { message = "success",
        code = 0,
        respData =
          Message
            { messageType = 500,
              info =
                Info
                  { text = "body"
                  }
            }
      }

startServer :: IO ()
startServer = do
  putStrLn "服务启动端口: 7700"
  run 7700 (serve api callback)
