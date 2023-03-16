{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Time.LocalTime
import OpenAI.Client
import Types.Req
import qualified Types.Resp as Resp
import Http.Server

main :: IO ()
main = do
  http  
  result <-
    chatCompletion
      Chat
        { stream = False,
          model = "gpt-3.5-turbo",
          messages = [Message {role = "user", content = "你好"}],
          temperature = 0.7
        }
  case result of
    Left err -> putStrLn err
    Right x -> putStrLn $ Resp.content $ Resp.message $ head (Resp.choices x)
  getZonedTime >>= print