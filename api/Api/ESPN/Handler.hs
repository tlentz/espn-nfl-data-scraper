{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.ESPN.Handler
    ( getGames
    , getGame
    ) where

import Data.String.Conversions (cs)
import Network.HTTP.Client
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Data.Text hiding (map, head, length)
import Servant (Handler)        
import Control.Monad.IO.Class (liftIO)
import Api.NFL.Types (TeamInfo(..), Scores(..), GameStats(..), Team(..), Game(..), defaultTeam)
import Data.Either
import Data.Either.Unwrap hiding (isLeft, isRight)

makeRequest :: Manager -> Text -> IO Text
makeRequest manager url = do
    request <- parseRequest (cs $ url)
    response <- httpLbs request manager
    responseClose response
    return $ cs $ responseBody response

getGames :: Int -> Int -> Handler [Game]
getGames year week =  do
    let url = buildWeekUrl year week
    manager <- liftIO $ newManager defaultManagerSettings
    gameIds <- liftIO $ getGameIds =<< (makeRequest manager url)
    games <- liftIO $ scrapeGames manager gameIds
    return games

getGame :: Text -> Handler Game
getGame gameId = do
    manager <- liftIO $ newManager defaultManagerSettings
    game <- liftIO $ scrapeGame manager gameId
    return game

buildWeekUrl :: Int -> Int -> Text
buildWeekUrl year week = "http://www.espn.com/nfl/schedule/" `append` "_/week/" `append` (cs $ show week) `append` "/year/" `append` (cs $ show year)

getMatchupUrl :: Text -> Text
getMatchupUrl gameId = "http://www.espn.com/nfl/matchup?gameId=" `append` gameId

getGameIds :: Text -> IO [Text]
getGameIds src = do
    let html = parseHtml $ cs src
    links <-
        runX $
        html >>>
        css "#schedule-page div a[name=&lpos=nfl:schedule:score]" ! "href"
    let 
        links' = map pack links
        gameIds = map (replace "/nfl/game?gameId=" "") links'
    return gameIds

scrapeGames :: Manager -> [Text] -> IO [Game]
scrapeGames manager gameIds = mapM (scrapeGame manager) gameIds

scrapeGame :: Manager -> Text -> IO Game
scrapeGame manager gameId = do
    src <- makeRequest manager (getMatchupUrl gameId)
    let html = parseHtml $ cs src
    boxHeader <- runX . xshow $ html >>> css "div.competitors"
    gameStats <-
        runX . xshow $
        html >>> css "article#teamstats-wrap table.mod-data tbody"
    teamInfo <- getTeamInfo $ cs $ head boxHeader
    scores <- getScores $ cs $ head boxHeader
    stats <- getGameStats $ head gameStats
    if isLeft teamInfo || isLeft scores || isLeft stats
    then return $ Game {gId = gameId, gHomeTeam = defaultTeam, gAwayTeam = defaultTeam}
    else do
        let away =
                Team
                { tInfo = fst $ fromRight teamInfo
                , tGameStats = fst $ fromRight stats
                , tScores = fst $ fromRight scores
                }
            home =
                Team
                { tInfo = snd $ fromRight teamInfo
                , tGameStats = snd $ fromRight stats
                , tScores = snd $ fromRight scores
                }
            game = Game {gId = gameId, gHomeTeam = home, gAwayTeam = away}
        return game

getTeamInfo :: Text -> IO (Either Text (TeamInfo, TeamInfo))
getTeamInfo src = do
    let html = parseHtml $ cs src
    teams <-
        runX $
        html >>>
        (css "div.away a.team-name" <+>
         css "div.away div.team-name" <+>
         css "div.home a.team-name" <+> css "div.home div.team-name") //>
        getText
    if length teams /= 6
        then return $ Left "Length does not equal 6"
        else do
            let 
                teams' = map pack teams
                away = TeamInfo (teams' !! 0) (teams' !! 1) (teams' !! 2) False
                home = TeamInfo (teams' !! 3) (teams' !! 4) (teams' !! 5) True
            return $ Right (away, home)

getScores :: Text -> IO (Either Text (Scores, Scores))
getScores src = do
    let html = parseHtml $ cs src
    scores <- runX $ html >>> (css "table#linescore tbody tr td") //> getText
    let lenScores = length scores
    case lenScores of
        12 -> do
            let away =
                    Scores
                        (read $ scores !! 1)
                        (read $ scores !! 2)
                        (read $ scores !! 3)
                        (read $ scores !! 4)
                        Nothing
                        (read $ scores !! 5)
                home =
                    Scores
                        (read $ scores !! 7)
                        (read $ scores !! 8)
                        (read $ scores !! 9)
                        (read $ scores !! 10)
                        Nothing
                        (read $ scores !! 11)
            return $ Right (away, home)
        14 -> do
            let away =
                    Scores
                        (read $ scores !! 1)
                        (read $ scores !! 2)
                        (read $ scores !! 3)
                        (read $ scores !! 4)
                        (Just $ read $ scores !! 5)
                        (read $ scores !! 6)
                home =
                    Scores
                        (read $ scores !! 8)
                        (read $ scores !! 9)
                        (read $ scores !! 10)
                        (read $ scores !! 11)
                        (Just $ read $ scores !! 12)
                        (read $ scores !! 13)
            return $ Right (away, home)
        _ -> return $ Left "Length does not equal 12 or 14"

getGameStats :: String -> IO (Either String (GameStats, GameStats))
getGameStats src = do
    let html = parseHtml $ cs src
    stats' <-
        (runX $ html >>> (css "tbody tr td") //> getText)
    let stats'' = map pack stats'
        stats = map strip stats''
    if length stats < 74
        then return $ Left "Length is left than 74"
        else do
            let totalFirstDowns =
                    (read $ cs $ stats !! 1, read $ cs $ stats !! 2)
                passingFirstDowns =
                    (read $ cs $ stats !! 4, read $ cs $ stats !! 5)
                rushingFirstDowns =
                    (read $ cs $ stats !! 7, read $ cs $ stats !! 8)
                firstDownsFromPenalties =
                    (read $ cs $ stats !! 10, read $ cs $ stats !! 11)
                thirdDownEfficiencyRatio =
                    ( parseRatio2IntTuple $ stats !! 13
                    , parseRatio2IntTuple $ stats !! 14)
                thirdDownsMade =
                    (fst $ fst thirdDownEfficiencyRatio, snd $ fst thirdDownEfficiencyRatio)
                thirdDownAttempts =
                    (fst $ snd thirdDownEfficiencyRatio, snd $ snd thirdDownEfficiencyRatio)
                thirdDownEfficiencyPct =
                    ( calculateRatioPercentage $ fst thirdDownEfficiencyRatio
                    , calculateRatioPercentage $ snd thirdDownEfficiencyRatio)
                fourthDownEfficiencyRatio =
                    ( parseRatio2IntTuple $ stats !! 16
                    , parseRatio2IntTuple $ stats !! 17)
                fourthDownsMade =
                    (fst $ fst fourthDownEfficiencyRatio, snd $ fst fourthDownEfficiencyRatio)
                fourthDownAttempts =
                    (fst $ snd fourthDownEfficiencyRatio, snd $ snd fourthDownEfficiencyRatio)
                fourthDownEfficiencyPct =
                    ( calculateRatioPercentage $ fst fourthDownEfficiencyRatio
                    , calculateRatioPercentage $ snd fourthDownEfficiencyRatio)
                totalPlays =
                    (read $ cs $ stats !! 19, read $ cs $ stats !! 20)
                totalYards =
                    (read $ cs $ stats !! 22, read $ cs $ stats !! 23)
                totalDrives =
                    (read $ cs $ stats !! 25, read $ cs $ stats !! 26)
                yardsPerPlay =
                    (read $ cs $ stats !! 28, read $ cs $ stats !! 29)
                totalPassingYards =
                    (read $ cs $ stats !! 31, read $ cs $ stats !! 32)
                completionAttemptRatio =
                    ( parseRatio2IntTuple $ stats !! 34
                    , parseRatio2IntTuple $ stats !! 35)
                passAttempts =
                    (fst $ fst completionAttemptRatio, snd $ fst completionAttemptRatio)
                passCompletions =
                    (fst $ snd completionAttemptRatio, snd $ snd completionAttemptRatio)
                completionPct =
                    ( calculateRatioPercentage $ fst completionAttemptRatio
                    , calculateRatioPercentage $ snd completionAttemptRatio)
                yardsPerPass =
                    (read $ cs $ stats !! 37, read $ cs $ stats !! 38)
                interceptionsThrown =
                    (read $ cs $ stats !! 40, read $ cs $ stats !! 41)
                sacksYardsLost =
                    ( parseRatio2IntTuple $ stats !! 43
                    , parseRatio2IntTuple $ stats !! 44)
                totalSacks =
                    (fst $ fst sacksYardsLost, snd $ fst sacksYardsLost)
                yardsLostFromSacks =
                    (fst $ snd sacksYardsLost, snd $ snd sacksYardsLost)
                totalRushingYards =
                    (read $ cs $ stats !! 46, read $ cs $ stats !! 47)
                rushingAttempts =
                    (read $ cs $ stats !! 49, read $ cs $ stats !! 50)
                yardsPerRush =
                    (read $ cs $ stats !! 52, read $ cs $ stats !! 53)
                redZoneMadeAttemptRatio =
                    ( parseRatio2IntTuple $ stats !! 55
                    , parseRatio2IntTuple $ stats !! 56)
                redZoneMade =
                    (fst $ fst redZoneMadeAttemptRatio, snd $ fst redZoneMadeAttemptRatio)
                redZoneAttempts =
                    (fst $ snd redZoneMadeAttemptRatio, snd $ snd redZoneMadeAttemptRatio)
                redZoneMadePct =
                    ( calculateRatioPercentage $ fst redZoneMadeAttemptRatio
                    , calculateRatioPercentage $ snd redZoneMadeAttemptRatio)
                penalties =
                    ( parseRatio2IntTuple $ stats !! 58
                    , parseRatio2IntTuple $ stats !! 59)
                totalPenalties =
                    (fst $ fst penalties, snd $ fst penalties)
                yardsGivenFromPenalties =
                    (fst $ snd penalties, snd $ snd penalties)
                turnovers = (read $ cs $ stats !! 61, read $ cs $ stats !! 62)
                fumblesLost =
                    (read $ cs $ stats !! 64, read $ cs $ stats !! 65)
                defenseSpecialTeamsTD =
                    (read $ cs $ stats !! 70, read $ cs $ stats !! 71)
                timeOfPossession =
                    ( parseTimeOfPossession $ stats !! 73
                    , parseTimeOfPossession $ stats !! 74)
                away =
                    GameStats
                    { totalFirstDowns = fst totalFirstDowns
                    , passingFirstDowns = fst passingFirstDowns
                    , rushingFirstDowns = fst rushingFirstDowns
                    , firstDownsFromPenalties = fst firstDownsFromPenalties
                    , thirdDownsMade = fst thirdDownsMade
                    , thirdDownAttempts = fst thirdDownAttempts
                    , thirdDownEfficiencyPct = fst thirdDownEfficiencyPct
                    , fourthDownsMade = fst fourthDownsMade
                    , fourthDownAttempts = fst fourthDownAttempts
                    , fourthDownEfficiencyPct = fst fourthDownEfficiencyPct
                    , totalPlays = fst totalPlays
                    , totalYards = fst totalYards
                    , totalDrives = fst totalDrives
                    , yardsPerPlay = fst yardsPerPlay
                    , totalPassingYards = fst totalPassingYards
                    , passAttempts = fst passAttempts
                    , passCompletions = fst passCompletions
                    , completionPct = fst completionPct
                    , yardsPerPass = fst yardsPerPass
                    , interceptionsThrown = fst interceptionsThrown
                    , totalSacks = fst totalSacks
                    , yardsLostFromSacks = fst yardsLostFromSacks
                    , totalRushingYards = fst totalRushingYards
                    , rushingAttempts = fst rushingAttempts
                    , yardsPerRush = fst yardsPerRush
                    , redZoneMade = fst redZoneMade
                    , redZoneAttempts = fst redZoneAttempts
                    , redZoneMadePct = fst redZoneMadePct
                    , totalPenalties = fst totalPenalties
                    , yardsGivenFromPenalties = fst yardsGivenFromPenalties
                    , turnovers = fst turnovers
                    , fumblesLost = fst fumblesLost
                    , defenseSpecialTeamsTD = fst defenseSpecialTeamsTD
                    , timeOfPossession = fst timeOfPossession
                    }
                home =
                    GameStats
                    { totalFirstDowns = snd totalFirstDowns
                    , passingFirstDowns = snd passingFirstDowns
                    , rushingFirstDowns = snd rushingFirstDowns
                    , firstDownsFromPenalties = snd firstDownsFromPenalties
                    , thirdDownsMade = snd thirdDownsMade
                    , thirdDownAttempts = snd thirdDownAttempts
                    , thirdDownEfficiencyPct = snd thirdDownEfficiencyPct
                    , fourthDownsMade = snd fourthDownsMade
                    , fourthDownAttempts = snd fourthDownAttempts
                    , fourthDownEfficiencyPct = snd fourthDownEfficiencyPct
                    , totalPlays = snd totalPlays
                    , totalYards = snd totalYards
                    , totalDrives = snd totalDrives
                    , yardsPerPlay = snd yardsPerPlay
                    , totalPassingYards = snd totalPassingYards
                    , passAttempts = snd passAttempts
                    , passCompletions = snd passCompletions
                    , completionPct = snd completionPct
                    , yardsPerPass = snd yardsPerPass
                    , interceptionsThrown = snd interceptionsThrown
                    , totalSacks = snd totalSacks
                    , yardsLostFromSacks = snd yardsLostFromSacks
                    , totalRushingYards = snd totalRushingYards
                    , rushingAttempts = snd rushingAttempts
                    , yardsPerRush = snd yardsPerRush
                    , redZoneMade = snd redZoneMade
                    , redZoneAttempts = snd redZoneAttempts
                    , redZoneMadePct = snd redZoneMadePct
                    , totalPenalties = snd totalPenalties
                    , yardsGivenFromPenalties = snd yardsGivenFromPenalties
                    , turnovers = snd turnovers
                    , fumblesLost = snd fumblesLost
                    , defenseSpecialTeamsTD = snd defenseSpecialTeamsTD
                    , timeOfPossession = snd timeOfPossession
                    }
            return $ Right (away, home)

parseRatio2IntTuple :: Text -> (Int, Int)
parseRatio2IntTuple str = (read $ cs $ parts !! 0, read $ cs $ parts !! 1)
  where
    parts = splitOn "-" str

parseTimeOfPossession :: Text -> Int
parseTimeOfPossession str = timeOfPossession
  where
    parts = splitOn ":" str
    minutes = read $ cs $ parts !! 0
    seconds = read $ cs $ parts !! 1
    timeOfPossession = 60 * minutes + seconds

calculateRatioPercentage :: (Int, Int) -> Double
calculateRatioPercentage (l,0) = 0
calculateRatioPercentage (l, r) = percentage
  where
    percentage = 100 * ((fromIntegral l) / (fromIntegral r))