{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types
    ( Api
    , Game(..)
    , Team(..)
    , TeamInfo(..)
    , GameStats(..)
    , Scores(..)
    ) where

import Data.Aeson (FromJSON, ToJSON, (.!=), (.:), (.:?), eitherDecode, encode, parseJSON, toJSON, withObject, defaultOptions, genericToJSON)
import qualified Data.Map.Strict as Map
import Elm (ElmType)
import GHC.Generics (Generic)
import Servant ((:<|>), (:>), ReqBody, Post, Get, JSON, QueryParam)
import Api.Helper (string2Int, string2Double)

type Api = "games" :> QueryParam "year" String :> QueryParam "week" String :> Get '[JSON] [Game]

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
    } deriving (Show, Generic, ElmType, ToJSON)

instance FromJSON TeamInfo where
    parseJSON =
        withObject "teamInfo" $ \o -> do
            longName <- o .:? "longName" .!= "unknown"
            shortName <- o .:? "shortName" .!= "unknown"
            abbrev <- o .:? "abbrev" .!= "unknown"
            isHome <- o .:? "isHome" .!= False
            return TeamInfo{..}

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
    } deriving (Show, Generic, ElmType, ToJSON)

instance FromJSON GameStats where
    parseJSON =
        withObject "gamestats" $ \o -> do
            totalFirstDowns <- o .:? "totalFirstDowns" .!= (-1)
            passingFirstDowns <- o .:? "passingFirstDowns" .!= (-1)
            rushingFirstDowns <- o .:? "rushingFirstDowns" .!= (-1)
            firstDownsFromPenalties <- o .:? "firstDownsFromPenalties" .!= (-1)
            thirdDownEfficiencyRatio <- o .:? "thirdDownEfficiencyRatio" .!= (-1,-1)
            thirdDownEfficiencyPct <- o .:? "thirdDownEfficiencyPct" .!= (-1.0)
            fourthDownEfficiencyRatio <- o .:? "fourthDownEfficiencyRatio" .!= (-1,-1)
            fourthDownEfficiencyPct <- o .:? "fourthDownEfficiencyPct" .!= (-1.0)
            totalPlays <- o .:? "totalPlays" .!= (-1)
            totalYards <- o .:? "totalYards" .!= (-1)
            totalDrives <- o .:? "totalDrives" .!= (-1)
            yardsPerPlay <- o .:? "yardsPerPlay" .!= (-1.0)
            completionAttemptRatio <- o .:? "completionAttemptRatio" .!= (-1,-1)
            completionPct <- o .:? "completionPct" .!= (-1.0)
            totalPassingYards <- o .:? "totalPassingYards" .!= (-1)
            yardsPerPass <- o .:? "yardsPerPass" .!= (-1.0)
            interceptionsThrown <- o .:? "interceptionsThrown" .!= (-1)
            sacksYardsLost <- o .:? "sacksYardsLost" .!= (-1,-1)
            totalRushingYards <- o .:? "totalRushingYards" .!= (-1)
            rushingAttempts <- o .:? "rushingAttempts" .!= (-1)
            yardsPerRush <- o .:? "yardsPerRush" .!= (-1.0)
            redZoneMadeAttemptRatio <- o .:? "redZoneMadeAttemptRatio" .!= (-1,-1)
            redZoneMadePct <- o .:? "redZoneMadePct" .!= (-1.0)
            penalties <- o .:? "penalties" .!= (-1,-1)
            turnovers <- o .:? "turnovers" .!= (-1)
            fumblesLost <- o .:? "fumblesLost" .!= (-1)
            defenseSpecialTeamsTD <- o .:? "defenseSpecialTeamsTD" .!= (-1)
            timeOfPossession <- o .:? "timeOfPossession" .!= (-1)
            return GameStats{..}


data Scores = Scores
    { q1 :: Int
    , q2 :: Int
    , q3 :: Int
    , q4 :: Int
    , ot :: Maybe Int
    , final :: Int
    } deriving (Show, Generic, ElmType, ToJSON)

instance FromJSON Scores where
    parseJSON =
        withObject "scores" $ \o -> do
            q1 <- o .:? "q1" .!= (-1)
            q2 <- o .:? "q2" .!= (-1)
            q3 <- o .:? "q3" .!= (-1)
            q4 <- o .:? "q4" .!= (-1)
            ot <- o .:? "ot" .!= Nothing
            final <- o .:? "final" .!= (-1)
            return Scores{..}