{-# LANGUAGE OverloadedStrings #-}

module Types.Configuration where

import Data.Text (Text)

data Configuration = Configuration
  { apiKey :: Text,
    organization :: Text
  }

createEmptyConfiguration :: Configuration
createEmptyConfiguration =
  Configuration
    { apiKey = "",
      organization = ""
    }