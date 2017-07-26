{-# LANGUAGE OverloadedStrings #-}

module Api.Lib
    ( buildUrls
    , buildWeekUrl
    , getGameIds
    , getGame
    , getGames
    , buildMatchupUrls
    , makeRequest
    , getTeamInfo
    , getScores
    , getGameStats
    ) where

import Api.Helper (string2Double, string2Int)
import Api.Types
import Data.Either
import Data.Either.Unwrap hiding (isLeft, isRight)
import Data.List.Split (splitOn)
import Data.String.Conversions (cs)
import Data.String.Utils (replace, strip)
import Network.HTTP.Client
import Text.HandsomeSoup
import Text.XML.HXT.Core

buildUrls :: String -> String -> String
buildUrls year week = buildWeekUrl year week

buildWeekUrl :: String -> String -> String
buildWeekUrl year week =
    concat
        ["http://www.espn.com/nfl/schedule/", "_/week/", week, "/year/", year]

getMatchupUrl :: String -> String
getMatchupUrl gameId = "http://www.espn.com/nfl/matchup?gameId=" ++ gameId

makeRequest :: Manager -> String -> IO String
makeRequest manager url = do
    request <- parseRequest url
    response <- httpLbs request manager
    responseClose response
    return $ cs $ responseBody response

getGameIds :: String -> IO [String]
getGameIds src = do
    let html = parseHtml $ cs src
    links <-
        runX $
        html >>>
        css "#schedule-page div a[name=&lpos=nfl:schedule:score]" ! "href"
    let gameIds = map (replace "/nfl/game?gameId=" "") links
    return gameIds

getGames :: Manager -> [String] -> IO [Either String Game]
getGames manager gameIds = mapM (getGame manager) gameIds

getGame :: Manager -> String -> IO (Either String Game)
getGame manager gameId = do
    let url = getMatchupUrl gameId
    src <- makeRequest manager url
    let html = parseHtml $ cs src
    boxHeader <- runX . xshow $ html >>> css "div.competitors"
    gameStats <-
        runX . xshow $
        html >>> css "article#teamstats-wrap table.mod-data tbody"
    teamInfo <- getTeamInfo $ head boxHeader
    scores <- getScores $ head boxHeader
    stats <- getGameStats $ head gameStats
    if isLeft teamInfo || isLeft scores || isLeft stats
        then return $ Left "not alright"
        else do
            let away =
                    Team
                    { info = fst $ fromRight teamInfo
                    , stats = fst $ fromRight stats
                    , scores = fst $ fromRight scores
                    }
                home =
                    Team
                    { info = snd $ fromRight teamInfo
                    , stats = snd $ fromRight stats
                    , scores = snd $ fromRight scores
                    }
                game = Game {gameId = gameId, teams = (away, home)}
            return $ Right game

buildMatchupUrls :: [String] -> [String]
buildMatchupUrls gameIds = matchupUrls
  where
    matchupUrls =
        map (replace "/nfl/game" "http://www.espn.com/nfl/matchup") gameIds

getTeamInfo :: String -> IO (Either String (TeamInfo, TeamInfo))
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
            let away = TeamInfo (teams !! 0) (teams !! 1) (teams !! 2) False
                home = TeamInfo (teams !! 3) (teams !! 4) (teams !! 5) True
            return $ Right (away, home)

getScores :: String -> IO (Either String (Scores, Scores))
getScores src = do
    let html = parseHtml $ cs src
    scores <- runX $ html >>> (css "table#linescore tbody tr td") //> getText
    let lenScores = length scores
    case lenScores of
        12 -> do
            let away =
                    Scores
                        (string2Int $ scores !! 1)
                        (string2Int $ scores !! 2)
                        (string2Int $ scores !! 3)
                        (string2Int $ scores !! 4)
                        Nothing
                        (string2Int $ scores !! 5)
                home =
                    Scores
                        (string2Int $ scores !! 7)
                        (string2Int $ scores !! 8)
                        (string2Int $ scores !! 9)
                        (string2Int $ scores !! 10)
                        Nothing
                        (string2Int $ scores !! 11)
            return $ Right (away, home)
        14 -> do
            let away =
                    Scores
                        (string2Int $ scores !! 1)
                        (string2Int $ scores !! 2)
                        (string2Int $ scores !! 3)
                        (string2Int $ scores !! 4)
                        (Just $ string2Int $ scores !! 5)
                        (string2Int $ scores !! 6)
                home =
                    Scores
                        (string2Int $ scores !! 8)
                        (string2Int $ scores !! 9)
                        (string2Int $ scores !! 10)
                        (string2Int $ scores !! 11)
                        (Just $ string2Int $ scores !! 12)
                        (string2Int $ scores !! 13)
            return $ Right (away, home)
        _ -> return $ Left "Length does not equal 12 or 14"

getGameStats :: String -> IO (Either String (GameStats, GameStats))
getGameStats src = do
    let html = parseHtml $ cs src
    stats <-
        (runX $ html >>> (css "tbody tr td") //> getText) >>= return . map strip
    if length stats < 74
        then return $ Left "Length is left than 74"
        else do
            let totalFirstDowns =
                    (string2Int $ stats !! 1, string2Int $ stats !! 2)
                passingFirstDowns =
                    (string2Int $ stats !! 4, string2Int $ stats !! 5)
                rushingFirstDowns =
                    (string2Int $ stats !! 7, string2Int $ stats !! 8)
                firstDownsFromPenalties =
                    (string2Int $ stats !! 10, string2Int $ stats !! 11)
                thirdDownEfficiencyRatio =
                    ( parseRatio2IntTuple $ stats !! 13
                    , parseRatio2IntTuple $ stats !! 14)
                thirdDownEfficiencyPct =
                    ( calculateRatioPercentage $ fst thirdDownEfficiencyRatio
                    , calculateRatioPercentage $ snd thirdDownEfficiencyRatio)
                fourthDownEfficiencyRatio =
                    ( parseRatio2IntTuple $ stats !! 16
                    , parseRatio2IntTuple $ stats !! 17)
                fourthDownEfficiencyPct =
                    ( calculateRatioPercentage $ fst fourthDownEfficiencyRatio
                    , calculateRatioPercentage $ snd fourthDownEfficiencyRatio)
                totalPlays =
                    (string2Int $ stats !! 19, string2Int $ stats !! 20)
                totalYards =
                    (string2Int $ stats !! 22, string2Int $ stats !! 23)
                totalDrives =
                    (string2Int $ stats !! 25, string2Int $ stats !! 26)
                yardsPerPlay =
                    (string2Double $ stats !! 28, string2Double $ stats !! 29)
                totalPassingYards =
                    (string2Int $ stats !! 31, string2Int $ stats !! 32)
                completionAttemptRatio =
                    ( parseRatio2IntTuple $ stats !! 34
                    , parseRatio2IntTuple $ stats !! 35)
                completionPct =
                    ( calculateRatioPercentage $ fst completionAttemptRatio
                    , calculateRatioPercentage $ snd completionAttemptRatio)
                yardsPerPass =
                    (string2Double $ stats !! 37, string2Double $ stats !! 38)
                interceptionsThrown =
                    (string2Int $ stats !! 40, string2Int $ stats !! 41)
                sacksYardsLost =
                    ( parseRatio2IntTuple $ stats !! 43
                    , parseRatio2IntTuple $ stats !! 44)
                totalRushingYards =
                    (string2Int $ stats !! 46, string2Int $ stats !! 47)
                rushingAttempts =
                    (string2Int $ stats !! 49, string2Int $ stats !! 50)
                yardsPerRush =
                    (string2Double $ stats !! 52, string2Double $ stats !! 53)
                redZoneMadeAttemptRatio =
                    ( parseRatio2IntTuple $ stats !! 55
                    , parseRatio2IntTuple $ stats !! 56)
                redZoneMadePct =
                    ( calculateRatioPercentage $ fst redZoneMadeAttemptRatio
                    , calculateRatioPercentage $ snd redZoneMadeAttemptRatio)
                penalties =
                    ( parseRatio2IntTuple $ stats !! 58
                    , parseRatio2IntTuple $ stats !! 59)
                turnovers = (string2Int $ stats !! 61, string2Int $ stats !! 62)
                fumblesLost =
                    (string2Int $ stats !! 64, string2Int $ stats !! 65)
                defenseSpecialTeamsTD =
                    (string2Int $ stats !! 70, string2Int $ stats !! 71)
                timeOfPossession =
                    ( parseTimeOfPossession $ stats !! 73
                    , parseTimeOfPossession $ stats !! 74)
                away =
                    GameStats
                    { totalFirstDowns = fst totalFirstDowns
                    , passingFirstDowns = fst passingFirstDowns
                    , rushingFirstDowns = fst rushingFirstDowns
                    , firstDownsFromPenalties = fst firstDownsFromPenalties
                    , thirdDownEfficiencyRatio = fst thirdDownEfficiencyRatio
                    , thirdDownEfficiencyPct = fst thirdDownEfficiencyPct
                    , fourthDownEfficiencyRatio = fst fourthDownEfficiencyRatio
                    , fourthDownEfficiencyPct = fst fourthDownEfficiencyPct
                    , totalPlays = fst totalPlays
                    , totalYards = fst totalYards
                    , totalDrives = fst totalDrives
                    , yardsPerPlay = fst yardsPerPlay
                    , totalPassingYards = fst totalPassingYards
                    , completionAttemptRatio = fst completionAttemptRatio
                    , completionPct = fst completionPct
                    , yardsPerPass = fst yardsPerPass
                    , interceptionsThrown = fst interceptionsThrown
                    , sacksYardsLost = fst sacksYardsLost
                    , totalRushingYards = fst totalRushingYards
                    , rushingAttempts = fst rushingAttempts
                    , yardsPerRush = fst yardsPerRush
                    , redZoneMadeAttemptRatio = fst redZoneMadeAttemptRatio
                    , redZoneMadePct = fst redZoneMadePct
                    , penalties = fst penalties
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
                    , thirdDownEfficiencyRatio = snd thirdDownEfficiencyRatio
                    , thirdDownEfficiencyPct = snd thirdDownEfficiencyPct
                    , fourthDownEfficiencyRatio = snd fourthDownEfficiencyRatio
                    , fourthDownEfficiencyPct = snd fourthDownEfficiencyPct
                    , totalPlays = snd totalPlays
                    , totalYards = snd totalYards
                    , totalDrives = snd totalDrives
                    , yardsPerPlay = snd yardsPerPlay
                    , totalPassingYards = snd totalPassingYards
                    , completionAttemptRatio = snd completionAttemptRatio
                    , completionPct = snd completionPct
                    , yardsPerPass = snd yardsPerPass
                    , interceptionsThrown = snd interceptionsThrown
                    , sacksYardsLost = snd sacksYardsLost
                    , totalRushingYards = snd totalRushingYards
                    , rushingAttempts = snd rushingAttempts
                    , yardsPerRush = snd yardsPerRush
                    , redZoneMadeAttemptRatio = snd redZoneMadeAttemptRatio
                    , redZoneMadePct = snd redZoneMadePct
                    , penalties = snd penalties
                    , turnovers = snd turnovers
                    , fumblesLost = snd fumblesLost
                    , defenseSpecialTeamsTD = snd defenseSpecialTeamsTD
                    , timeOfPossession = snd timeOfPossession
                    }
            return $ Right (away, home)

parseRatio2IntTuple :: String -> (Int, Int)
parseRatio2IntTuple str = (string2Int $ parts !! 0, string2Int $ parts !! 1)
  where
    parts = splitOn "-" str

parseTimeOfPossession :: String -> Int
parseTimeOfPossession str = timeOfPossession
  where
    parts = splitOn ":" str
    minutes = string2Int $ parts !! 0
    seconds = string2Int $ parts !! 1
    timeOfPossession = 60 * minutes + seconds

calculateRatioPercentage :: (Int, Int) -> Double
calculateRatioPercentage (left,0) = 0
calculateRatioPercentage (left, right) = percentage
  where
    percentage = 100 * ((fromIntegral left) / (fromIntegral right))
