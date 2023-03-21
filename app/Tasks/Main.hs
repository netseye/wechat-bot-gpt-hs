{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Redis
  ( Connection,
    Message (msgMessage),
    PubSub,
    connect,
    defaultConnectInfo,
    disconnect,
    pubSub,
    runRedis,
    setex,
    subscribe,
  )
import Feishu.Client (bitableAddRecord, bitableUpdateRecord)
-- import Feishu.Config (fromEnvVariables)
import Http.Types (BotReq (..))
import OpenAI.Client (chatCompletion)
import qualified Types.Req as Req
import qualified Types.Resp as Resp
import Wechat.Message

setAuthToken :: Connection -> (Text, Integer) -> IO ()
setAuthToken conn y = do
  runRedis conn $ void $ setex "authtoken" (snd y) $ T.encodeUtf8 (fst y)

main :: IO ()
main = do
  -- 连接到Redis服务器
  conn <- connect defaultConnectInfo

  putStrLn "任务队列开始"
  -- 订阅两个频道
  runRedis conn $ pubSub (subscribe ["msg"]) handler

  -- 关闭连接
  disconnect conn

handler :: Message -> IO PubSub
handler msg = do
  void $ async $ do
    doTask
      ( decode $
          BSL.fromStrict $
            msgMessage msg
      )
      `catch` \(e :: SomeException) -> do
        print e
        return mempty
  return mempty

doTask :: Maybe BotReq -> IO PubSub
doTask req = do
  case req of
    Nothing -> return mempty
    Just m -> do
      rid <- bitableAddRecord (receivedName m) (spoken m)
      case rid of
        Nothing -> return mempty
        Just d -> do
          r <- chatCompletion (Req.defaultChat (spoken m))
          let content = Resp.content $ Resp.message $ head $ Resp.choices (fromJust r)
          void $ bitableUpdateRecord d $ T.pack content
          sendMessage (unpack $ receivedName m) "worktool1" content
          return mempty