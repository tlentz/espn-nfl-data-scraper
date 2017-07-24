module Types
    ( Game (..)
    , Team (..)
    , TeamInfo (..)
    , GameStats (..)
    , Scores (..)
    ) where

data Game = Game 
  { gameId :: String
  , teams :: (Team, Team)
  } deriving (Show)

data Team = Team
  { info :: TeamInfo
  , stats :: GameStats
  , scores :: Scores
  } deriving (Show)

data TeamInfo = TeamInfo
  { longName :: String
  , shortName :: String
  , abbrev :: String
  , isHome :: Bool
  } deriving (Show)

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
  } deriving (Show)

data Scores = Scores
  { q1    :: Int
  , q2    :: Int
  , q3    :: Int
  , q4    :: Int
  , ot    :: Maybe Int
  , final :: Int 
  } deriving (Show)