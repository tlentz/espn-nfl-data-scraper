{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Config.Types
    ( Config(..)
    , defaultConfig
    ) where

import Database.PostgreSQL.Simple (ConnectInfo(..))

data Config = Config
    { postgresConfig :: ConnectInfo
    } deriving (Show)

pgConnectInfo :: ConnectInfo
pgConnectInfo = ConnectInfo
    "localhost"
    5432
    "nfl"
    "nfldb"
    "nfl"

defaultConfig :: Config
defaultConfig = Config pgConnectInfo