{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Http.Error where

import Data.Aeson (KeyValue ((.=)), object)
import Data.String.Conversions (cs)
import Network.Wai (Request (rawPathInfo))
import Servant
  ( ErrorFormatter,
    ErrorFormatters (bodyParserErrorFormatter, notFoundErrorFormatter),
    JSON,
    NotFoundErrorFormatter,
    Proxy (..),
    ServerError (errBody, errHeaders),
    defaultErrorFormatters,
    err400,
    err404,
    getAcceptHeader,
  )
import Servant.API.ContentTypes
  ( AllCTRender (handleAcceptH),
  )

customFormatter :: ErrorFormatter
customFormatter tr req err =
  let -- aeson Value which will be sent to the client
      value = object ["combinator" .= show tr, "error" .= err]
      -- Accept header of the request
      accH = getAcceptHeader req
   in -- handleAcceptH is Servant's function that checks whether the client can accept a
      -- certain message type.
      -- In this case we call it with "Proxy '[JSON]" argument, meaning that we want to return a JSON.
      case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
        -- If client can't handle JSON, we just return the body the old way
        Nothing -> err400 {errBody = cs err}
        -- Otherwise, we return the JSON formatted body and set the "Content-Type" header.
        Just (ctypeH, body) ->
          err400
            { errBody = body,
              errHeaders = [("Content-Type", cs ctypeH)]
            }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req =
  err404
    { errBody = cs $ "{\"message\":\"Not found\", \"code\":\"404\", \"path\": \"" <> rawPathInfo req <> "\"}",
      errHeaders = [("Content-Type", "application/json")]
    }

customFormatters :: ErrorFormatters
customFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = customFormatter,
      notFoundErrorFormatter = notFoundFormatter
    }

-- notFoundMessage :: String -> ByteString
-- notFoundMessage msg = BSLI.toStrict $ encode Resp {message = msg, code = 404, respData = Nothing}