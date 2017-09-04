{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.NFL.Types
    ( Game(..)
    , Team(..)
    , TeamInfo(..)
    , GameStats(..)
    , Scores(..)
    , defaultTeam
    ) where

import Data.Aeson (FromJSON, ToJSON, (.!=), (.:?), parseJSON, withObject)
import Elm (ElmType)
import GHC.Generics (Generic)
import Data.Text

data Game = Game
    { gId :: Text
    , gHomeTeam :: Team
    , gAwayTeam :: Team
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Team = Team
    { tInfo :: TeamInfo
    , tGameStats :: GameStats
    , tScores :: Scores
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

defaultTeam :: Team
defaultTeam = Team
    defaultTeamInfo
    defaultGameStats
    defaultScores

data TeamInfo = TeamInfo
    { tiLongName :: Text
    , tiShortName :: Text
    , tiAbbrev :: Text
    , tiIsHome :: Bool
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

defaultTeamInfo :: TeamInfo
defaultTeamInfo = TeamInfo
    { tiLongName = "unknown"
    , tiShortName = "unknown"
    , tiAbbrev = "unknown"
    , tiIsHome = False
    }

data GameStats = GameStats
    { totalFirstDowns :: Int
    , passingFirstDowns :: Int
    , rushingFirstDowns :: Int
    , firstDownsFromPenalties :: Int
    , thirdDownsMade :: Int
    , thirdDownAttempts :: Int
    , thirdDownEfficiencyPct :: Double
    , fourthDownsMade :: Int
    , fourthDownAttempts :: Int
    , fourthDownEfficiencyPct :: Double
    , totalPlays :: Int
    , totalYards :: Int
    , totalDrives :: Int
    , yardsPerPlay :: Double
    , passAttempts :: Int
    , passCompletions :: Int
    , completionPct :: Double
    , totalPassingYards :: Int
    , yardsPerPass :: Double
    , interceptionsThrown :: Int
    , totalSacks :: Int
    , yardsLostFromSacks :: Int
    , totalRushingYards :: Int
    , rushingAttempts :: Int
    , yardsPerRush :: Double
    , redZoneMade :: Int
    , redZoneAttempts :: Int
    , redZoneMadePct :: Double
    , totalPenalties :: Int
    , yardsGivenFromPenalties :: Int
    , turnovers :: Int
    , fumblesLost :: Int
    , defenseSpecialTeamsTD :: Int
    , timeOfPossession :: Int
    } deriving (Show, Generic, ElmType, ToJSON)

defaultGameStats :: GameStats
defaultGameStats = GameStats
    { totalFirstDowns = -1
    , passingFirstDowns = -1
    , rushingFirstDowns = -1
    , firstDownsFromPenalties = -1
    , thirdDownsMade = -1
    , thirdDownAttempts = -1
    , thirdDownEfficiencyPct = -1.0
    , fourthDownsMade = -1
    , fourthDownAttempts = -1
    , fourthDownEfficiencyPct = -1.0
    , totalPlays = -1
    , totalYards = -1
    , totalDrives = -1
    , yardsPerPlay = -1.0
    , passAttempts = -1
    , passCompletions = -1
    , completionPct = -1.0
    , totalPassingYards = -1
    , yardsPerPass = -1.0
    , interceptionsThrown = -1
    , totalSacks = -1
    , yardsLostFromSacks = -1
    , totalRushingYards = -1
    , rushingAttempts = -1
    , yardsPerRush = -1.0
    , redZoneMade = -1
    , redZoneAttempts = -1
    , redZoneMadePct = -1.0
    , totalPenalties = -1
    , yardsGivenFromPenalties = -1
    , turnovers = -1
    , fumblesLost = -1
    , defenseSpecialTeamsTD = -1
    , timeOfPossession = -1
    }

instance FromJSON GameStats where
    parseJSON =
        withObject "gamestats" $ \o -> do
            totalFirstDowns <- o .:? "totalFirstDowns" .!= (-1)
            passingFirstDowns <- o .:? "passingFirstDowns" .!= (-1)
            rushingFirstDowns <- o .:? "rushingFirstDowns" .!= (-1)
            firstDownsFromPenalties <- o .:? "firstDownsFromPenalties" .!= (-1)
            thirdDownsMade <- o .:? "thirdDownsMade" .!= (-1)
            thirdDownAttempts <- o .:? "thirdDownAttempts" .!= (-1)
            thirdDownEfficiencyPct <- o .:? "thirdDownEfficiencyPct" .!= (-1.0)
            fourthDownsMade <- o .:? "fourthDownsMade" .!= (-1)
            fourthDownAttempts <- o .:? "fourthDownAttempts" .!= (-1)
            fourthDownEfficiencyPct <- o .:? "fourthDownEfficiencyPct" .!= (-1.0)
            totalPlays <- o .:? "totalPlays" .!= (-1)
            totalYards <- o .:? "totalYards" .!= (-1)
            totalDrives <- o .:? "totalDrives" .!= (-1)
            yardsPerPlay <- o .:? "yardsPerPlay" .!= (-1.0)
            passAttempts <- o .:? "passAttempts" .!= (-1)
            passCompletions <- o .:? "passCompletions" .!= (-1)
            completionPct <- o .:? "completionPct" .!= (-1.0)
            totalPassingYards <- o .:? "totalPassingYards" .!= (-1)
            yardsPerPass <- o .:? "yardsPerPass" .!= (-1.0)
            interceptionsThrown <- o .:? "interceptionsThrown" .!= (-1)
            totalSacks <- o .:? "totalSacks" .!= (-1)
            yardsLostFromSacks <- o .:? "yardsLostFromSacks" .!= (-1)
            totalRushingYards <- o .:? "totalRushingYards" .!= (-1)
            rushingAttempts <- o .:? "rushingAttempts" .!= (-1)
            yardsPerRush <- o .:? "yardsPerRush" .!= (-1.0)
            redZoneMade <- o .:? "redZoneMade" .!= (-1)
            redZoneAttempts <- o .:? "redZoneAttempts" .!= (-1)
            redZoneMadePct <- o .:? "redZoneMadePct" .!= (-1.0)
            totalPenalties <- o .:? "totalPenalties" .!= (-1)
            yardsGivenFromPenalties <- o .:? "yardsGivenFromPenalties" .!= (-1)
            turnovers <- o .:? "turnovers" .!= (-1)
            fumblesLost <- o .:? "fumblesLost" .!= (-1)
            defenseSpecialTeamsTD <- o .:? "defenseSpecialTeamsTD" .!= (-1)
            timeOfPossession <- o .:? "timeOfPossession" .!= (-1)
            return GameStats{..}

data Scores = Scores
    { scoresQ1 :: Int
    , scoresQ2 :: Int
    , scoresQ3 :: Int
    , scoresQ4 :: Int
    , scoresOT :: Maybe Int
    , scoresFinal :: Int
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

defaultScores :: Scores
defaultScores = Scores
    { scoresQ1 = -1
    , scoresQ2 = -1
    , scoresQ3 = -1
    , scoresQ4 = -1
    , scoresOT = Just (-1)
    , scoresFinal = -1
    }