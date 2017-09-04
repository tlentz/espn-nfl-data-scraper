{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Types
    ( Api
    , ApiWithAssets
    ) where

import Data.Text (Text)
import Servant ((:<|>), (:>), Capture, Get, JSON, Post, ReqBody, Raw)
import Api.Example.Types (Dice)
import Api.NFL.Types (Game)


type Api
        = "api"
            :> ("getGames" :> Capture "year" Int
                            :> Capture "week" Int
                            :> Get '[JSON] [Game]
            :<|> "getGame" :> Capture "gameId" Text
                           :> Get '[JSON] Game
               )

type ApiWithAssets = "espn-nfl-data-scraper" :> (Api :<|> Raw)