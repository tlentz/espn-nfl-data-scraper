module Types exposing (..)
import Http
import Shared.Generated exposing (..)

type Msg 
    = YearInput String
    | WeekInput String
    | GameIdInput String
    | GetGames
    | GetGame
    | RecieveGames (List Game)
    | RecieveGame Game
    | Error Http.Error

type alias Model = 
    { games : List Game
    , game : Game
    , year : Int
    , week : Int
    , gameId : String
    , showGames : Bool
    , showGame : Bool
    }

initModel : Model
initModel = 
    { games = []
    , game = initGame
    , year = 0
    , week = 0
    , gameId = ""
    , showGames = False
    , showGame = False
    }

initGame : Game
initGame =
    { gId = ""
    , gAwayTeam = initTeam
    , gHomeTeam = initTeam
    }

initTeam : Team
initTeam =
    { tInfo = initTeamInfo
    , tGameStats = initGameStats
    , tScores = initScores
    }

initTeamInfo : TeamInfo
initTeamInfo =
    { tiLongName = "unknown"
    , tiShortName = "unknown"
    , tiAbbrev = "unknown"
    , tiIsHome = False
    }

initGameStats : GameStats
initGameStats =
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

initScores : Scores
initScores =
    { scoresQ1 = -1
    , scoresQ2 = -1
    , scoresQ3 = -1
    , scoresQ4 = -1
    , scoresOT = Just (-1)
    , scoresFinal = -1
    }
