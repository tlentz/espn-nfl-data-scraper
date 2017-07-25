{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Types
    ( Api
    , Game(..)
    , Team(..)
    , TeamInfo(..)
    , GameStats(..)
    , Scores(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import Elm (ElmType)
import GHC.Generics (Generic)
import Servant ((:<|>), (:>), ReqBody, Post, Get, JSON, QueryParam)

type Api = "games" :> QueryParam "year" String :> QueryParam "week" String :> Get '[ JSON] [Game]

data Game = Game
    { gameId :: String
    , teams :: (Team, Team)
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Team = Team
    { info :: TeamInfo
    , stats :: GameStats
    , scores :: Scores
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data TeamInfo = TeamInfo
    { longName :: String
    , shortName :: String
    , abbrev :: String
    , isHome :: Bool
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data GameStats = GameStats
    { totalFirstDowns :: Int
    , passingFirstDowns :: Int
    , rushingFirstDowns :: Int
    , firstDownsFromPenalties :: Int
    , thirdDownEfficiencyRatio :: (Int, Int)
    , thirdDownEfficiencyPct :: Double
    , fourthDownEfficiencyRatio :: (Int, Int)
    , fourthDownEfficiencyPct :: Double
    , totalPlays :: Int
    , totalYards :: Int
    , totalDrives :: Int
    , yardsPerPlay :: Double
    , completionAttemptRatio :: (Int, Int)
    , completionPct :: Double
    , totalPassingYards :: Int
    , yardsPerPass :: Double
    , interceptionsThrown :: Int
    , sacksYardsLost :: (Int, Int)
    , totalRushingYards :: Int
    , rushingAttempts :: Int
    , yardsPerRush :: Double
    , redZoneMadeAttemptRatio :: (Int, Int)
    , redZoneMadePct :: Double
    , penalties :: (Int, Int)
    , turnovers :: Int
    , fumblesLost :: Int
    , defenseSpecialTeamsTD :: Int
    , timeOfPossession :: Int
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Scores = Scores
    { q1 :: Int
    , q2 :: Int
    , q3 :: Int
    , q4 :: Int
    , ot :: Maybe Int
    , final :: Int
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)
