{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async (async)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
import Http.Types (BotReq (..))
import OpenAI.Client (chatCompletion)
import qualified Types.Req as Req
import qualified Types.Resp as Resp
import Utils.Logger (LogLevel (..), logger)
import Wechat.Message (getWorkToolId, sendMessage)

setAuthToken :: Connection -> (Text, Integer) -> IO ()
setAuthToken conn y = do
  runRedis conn $ void $ setex "authtoken" (snd y) $ TE.encodeUtf8 (fst y)

main :: IO ()
main = do
  -- 连接到Redis服务器
  conn <- connect defaultConnectInfo

  logger Info "任务队列开始"
  -- 订阅两个频道
  runRedis conn $ pubSub (subscribe ["msg"]) handler

  -- 关闭连接
  disconnect conn

handler :: Message -> IO PubSub
handler msg = do
  async (doTask task `catch` handleException) >> return mempty
  where
    task = decode (BSL.fromStrict (msgMessage msg))
    handleException :: SomeException -> IO PubSub
    handleException e = print e >> return mempty

doTask :: Maybe BotReq -> IO PubSub
doTask Nothing = return mempty
doTask (Just req)
  | atMe req == "false" || T.null (groupName req) = return mempty
  | otherwise = do
      rid <- bitableAddRecord (receivedName req) (spoken req)
      case rid of
        Nothing -> return mempty
        Just d -> do
          r <- chatCompletion (Req.defaultChat (spoken req))
          appid <- getWorkToolId
          let content = Resp.content $ Resp.message $ head $ Resp.choices (fromJust r)
          void $ bitableUpdateRecord d $ T.pack content
          sendMessage (T.unpack $ groupName req) (T.unpack $ receivedName req) (fromMaybe "" appid) content
          return mempty
