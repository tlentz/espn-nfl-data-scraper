{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Server
     ( server
     ) where

import Servant ((:<|>)((:<|>)), Server, serveDirectoryFileServer)
import Api.Types (ApiWithAssets)
import Api.ESPN.Handler (getGames, getGame)

server :: Server ApiWithAssets
server = (getGames :<|> getGame) :<|> serveStatic'
    where
        serveStatic' = serveDirectoryFileServer "./frontend/dist/static"