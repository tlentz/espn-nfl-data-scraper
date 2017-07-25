{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent.STM (TVar, newTVarIO)
import qualified Data.Map.Strict as Map
import Data.UUID (toString)
import Lucid
       (Html, body_, content_, doctypehtml_, head_, href_, link_, meta_,
        name_, rel_, script_, src_, title_)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
       ((:<|>)((:<|>)), (:>), Get, Proxy(Proxy), Raw, Server, serve,
        serveDirectory)
import Servant.HTML.Lucid (HTML)
import System.Random (randomIO)

import qualified Api.Server
import qualified Api.Types

type SiteApi = "api" :> Api.Types.Api :<|> Get '[ HTML] (Html ()) :<|> "assets" :> Raw

siteApi :: Proxy SiteApi
siteApi = Proxy

server :: Server SiteApi
server = apiServer :<|> home :<|> assets
  where
    home = return homePage
    apiServer = Api.Server.server
    assets = serveDirectory "frontend/dist"

homePage :: Html ()
homePage =
    doctypehtml_ $ do
        head_ $ do
            title_ "Example Servant-Elm App"
            meta_
                [ name_ "viewport"
                , content_ "width=device-width, initial-scale=1"
                ]
            link_
                [ rel_ "stylesheet"
                , href_
                      "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
                ]
            script_ [src_ "assets/app.js"] ""
        body_ (script_ "var elmApp = Elm.Main.fullscreen()")

app :: Application
app = serve siteApi server

main :: IO ()
main = do
    let port = 8000
    putStrLn $ "Serving on port " ++ show port ++ "..."
    run port app
