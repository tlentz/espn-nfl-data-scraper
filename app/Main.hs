{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Proxy(..), serve)
import Api.Server (server)
import Api.Types (ApiWithAssets)

apiWithAssets :: Proxy ApiWithAssets
apiWithAssets = Proxy

app :: Application
app = serve apiWithAssets server

main :: IO ()
main = do
  let port = 3000
  run port app