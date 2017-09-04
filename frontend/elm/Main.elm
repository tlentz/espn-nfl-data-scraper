module Main exposing (main)

import Html exposing (div, button, text, input, p, h1, h2, form, Html, ul, li, table, tr, td, thead, th, tbody)
import Html.Attributes exposing (placeholder, value, type_, class, style, disabled)
import Html.Events exposing (onClick, onInput)
import Shared.Generated exposing (..)
import Types exposing (..)
import Rest exposing (..)

main : Program Never Model Msg
main =
  Html.program
    { init = (initModel, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        YearInput year -> 
            let
                newYear = Result.withDefault 0 (String.toInt year)
            in
                ({model | year = newYear}, Cmd.none)
            
        WeekInput week ->
            let
                newWeek = Result.withDefault 0 (String.toInt week)
            in
                ({model | week = newWeek}, Cmd.none)

        GameIdInput gameId -> ({model | gameId = gameId}, Cmd.none)
        
        GetGame -> (model, getGame model.gameId)

        GetGames -> (model, getGames model.year model.week)

        RecieveGame game ->
          let
                temp = Debug.log "got stuff back" "asdf"
            in
                ({model | game = game, showGame = True, showGames = False}, Cmd.none)

        RecieveGames games -> 
            let
                temp = Debug.log "got stuff back" "asdf"
            in
                ({model | games = games, showGames = True, showGame = False}, Cmd.none)
        

        Error err ->  ({model | year = -1}, Cmd.none)


scores : Scores -> Html Msg
scores sc =
    let
        ot = Maybe.withDefault -1 sc.scoresOT
        otScores = case ot of
            -1 -> ""
            _ -> " OT: " ++ (toString ot)
    in
        div [] [ text <| "Q1:" ++ (toString sc.scoresQ1) ++ " Q2:"++ (toString sc.scoresQ2) ++ " Q3:"++(toString sc.scoresQ3) ++ " Q4:"++ (toString sc.scoresQ4) ++ otScores ++ " Final:" ++ (toString sc.scoresFinal) ]

displayStat : String -> Int -> Html Msg
displayStat title stat =
    div [] [text <| title ++ ": " ++ (toString stat)]


stats : GameStats -> Html Msg
stats gs =
    div []
        [ ul [] 
            [ li [] [displayStat "Total First Downs: " gs.totalFirstDowns]
            , li [] [displayStat "Passing First Downs: " gs.passingFirstDowns]
            , li [] [displayStat "Rushing First Downs: " gs.rushingFirstDowns]
            , li [] [displayStat "Total Plays: " gs.totalPlays]
            , li [] [displayStat "Total Yards: " gs.totalYards]
            , li [] [displayStat "Total Drives: " gs.totalDrives]
            , li [] [displayStat "Total Rushing Yards: " gs.totalRushingYards]
            , li [] [displayStat "Turnovers: " gs.turnovers]
            , li [] [displayStat "Time of Possesion: " gs.timeOfPossession]
            ]
        ]

displayTeamInfo : TeamInfo -> Html Msg
displayTeamInfo info =
    let
        whereAt = if info.tiIsHome then li [] [ text "Home" ] else li [] [ text "Away" ]
    in
        ul []
           [ li [] [ text <| info.tiLongName ++ info.tiShortName ]
           , li [] [ whereAt]
           ]

displayScores : Game -> Html Msg
displayScores game =
    let
        isOT = case game.gAwayTeam.tScores.scoresOT of
                    Nothing -> False
                    _ -> True
    in
        if isOT then
            displayOTScores game
        else
            table [] 
                [ thead []
                    [ th [] []
                    , th [] [ text "1" ]
                    , th [] [ text "2" ]
                    , th [] [ text "3" ]
                    , th [] [ text "4" ]
                    , th [] [ text "Final" ]
                    ]
                , tbody []
                    [ tr []
                        [ td [] [ text game.gAwayTeam.tInfo.tiAbbrev ]
                        , td [] [ text (toString game.gAwayTeam.tScores.scoresQ1) ]
                        , td [] [ text (toString game.gAwayTeam.tScores.scoresQ2) ]
                        , td [] [ text (toString game.gAwayTeam.tScores.scoresQ3) ]
                        , td [] [ text (toString game.gAwayTeam.tScores.scoresQ4) ]
                        , td [] [ text (toString game.gAwayTeam.tScores.scoresFinal) ]
                        ]
                    , tr []
                        [ td [] [ text game.gHomeTeam.tInfo.tiAbbrev ]
                        , td [] [ text (toString game.gHomeTeam.tScores.scoresQ1) ]
                        , td [] [ text (toString game.gHomeTeam.tScores.scoresQ2) ]
                        , td [] [ text (toString game.gHomeTeam.tScores.scoresQ3) ]
                        , td [] [ text (toString game.gHomeTeam.tScores.scoresQ4) ]
                        , td [] [ text (toString game.gHomeTeam.tScores.scoresFinal) ]
                        ]
                    ]
                ]

displayOTScores : Game -> Html Msg
displayOTScores game =
    table [] 
        [ thead []
            [ th [] []
            , th [] [ text "1" ]
            , th [] [ text "2" ]
            , th [] [ text "3" ]
            , th [] [ text "4" ]
            , th [] [ text "OT" ]
            , th [] [ text "Final" ]
            ]
        , tbody []
            [ tr []
                [ td [] [ text game.gAwayTeam.tInfo.tiAbbrev ]
                , td [] [ text (toString game.gAwayTeam.tScores.scoresQ1) ]
                , td [] [ text (toString game.gAwayTeam.tScores.scoresQ2) ]
                , td [] [ text (toString game.gAwayTeam.tScores.scoresQ3) ]
                , td [] [ text (toString game.gAwayTeam.tScores.scoresQ4) ]
                , td [] [ text (toString game.gAwayTeam.tScores.scoresOT) ]
                , td [] [ text (toString game.gAwayTeam.tScores.scoresFinal) ]
                ]
            , tr []
                [ td [] [ text game.gHomeTeam.tInfo.tiAbbrev ]
                , td [] [ text (toString game.gHomeTeam.tScores.scoresQ1) ]
                , td [] [ text (toString game.gHomeTeam.tScores.scoresQ2) ]
                , td [] [ text (toString game.gHomeTeam.tScores.scoresQ3) ]
                , td [] [ text (toString game.gHomeTeam.tScores.scoresQ4) ]
                , td [] [ text (toString game.gHomeTeam.tScores.scoresOT) ]
                , td [] [ text (toString game.gHomeTeam.tScores.scoresFinal) ]
                ]
            ]
        ]

displayTeamInfoAndScores : Game -> Html Msg
displayTeamInfoAndScores game =
    table []
          [ tr [] 
               [ td [] [ displayTeamInfo game.gAwayTeam.tInfo ]
               , td [] [ displayScores game ]
               , td [] [ displayTeamInfo game.gHomeTeam.tInfo ]
               ]
          ]

printGameTable : Game -> Html Msg
printGameTable game =
    let
        away = game.gAwayTeam.tGameStats
        home = game.gHomeTeam.tGameStats
    in
        table [] 
            [ thead []
                [ th [] [ text "Matchup" ]
                , th [] [ text game.gAwayTeam.tInfo.tiAbbrev ]
                , th [] [ text game.gHomeTeam.tInfo.tiAbbrev ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ text "Total First Downs" ]
                    , td [] [ text (toString away.totalFirstDowns) ]
                    , td [] [ text (toString home.totalFirstDowns) ]
                    ]
                , tr []
                    [ td [] [ text "Passing First Downs" ]
                    , td [] [ text (toString away.passingFirstDowns) ]
                    , td [] [ text (toString home.passingFirstDowns) ]
                    ]
                , tr []
                    [ td [] [ text "Rushing First Downs" ]
                    , td [] [ text (toString away.rushingFirstDowns) ]
                    , td [] [ text (toString home.rushingFirstDowns) ]
                    ]
                , tr []
                    [ td [] [ text "First Downs From Penalties" ]
                    , td [] [ text (toString away.firstDownsFromPenalties) ]
                    , td [] [ text (toString home.firstDownsFromPenalties) ]
                    ]
                , tr []
                    [ td [] [ text "Third Downs Made" ]
                    , td [] [ text (toString away.thirdDownsMade) ]
                    , td [] [ text (toString home.thirdDownsMade) ]
                    ]
                , tr []
                    [ td [] [ text "Third Down Attempts" ]
                    , td [] [ text (toString away.thirdDownAttempts) ]
                    , td [] [ text (toString home.thirdDownAttempts) ]
                    ]
                , tr []
                    [ td [] [ text "Third Down Efficiency" ]
                    , td [] [ text (toString away.thirdDownEfficiencyPct) ]
                    , td [] [ text (toString home.thirdDownEfficiencyPct) ]
                    ]
                , tr []
                    [ td [] [ text "Fourth Downs Made" ]
                    , td [] [ text (toString away.fourthDownsMade) ]
                    , td [] [ text (toString home.fourthDownsMade) ]
                    ]
                , tr []
                    [ td [] [ text "Fourth Down Attempts" ]
                    , td [] [ text (toString away.fourthDownAttempts) ]
                    , td [] [ text (toString home.fourthDownAttempts) ]
                    ]
                , tr []
                    [ td [] [ text "Fourth Down Efficiency" ]
                    , td [] [ text (toString away.fourthDownEfficiencyPct) ]
                    , td [] [ text (toString home.fourthDownEfficiencyPct) ]
                    ]
                , tr []
                    [ td [] [ text "Total Plays" ]
                    , td [] [ text (toString away.totalPlays) ]
                    , td [] [ text (toString home.totalPlays) ]
                    ]
                , tr []
                    [ td [] [ text "Total Yards" ]
                    , td [] [ text (toString away.totalYards) ]
                    , td [] [ text (toString home.totalYards) ]
                    ]
                , tr []
                    [ td [] [ text "Total Drives" ]
                    , td [] [ text (toString away.totalDrives) ]
                    , td [] [ text (toString home.totalDrives) ]
                    ]
                , tr []
                    [ td [] [ text "Yards Per Play" ]
                    , td [] [ text (toString away.yardsPerPlay) ]
                    , td [] [ text (toString home.yardsPerPlay) ]
                    ]
                , tr []
                    [ td [] [ text "Passing Attempts" ]
                    , td [] [ text (toString away.passAttempts) ]
                    , td [] [ text (toString home.passAttempts) ]
                    ]
                , tr []
                    [ td [] [ text "Passing Completions" ]
                    , td [] [ text (toString away.passCompletions) ]
                    , td [] [ text (toString home.passCompletions) ]
                    ]
                , tr []
                    [ td [] [ text "Completion Percentage" ]
                    , td [] [ text (toString away.completionPct) ]
                    , td [] [ text (toString home.completionPct) ]
                    ]
                , tr []
                    [ td [] [ text "Total Passing Yards" ]
                    , td [] [ text (toString away.totalPassingYards) ]
                    , td [] [ text (toString home.totalPassingYards) ]
                    ]
                , tr []
                    [ td [] [ text "Yards Per Pass" ]
                    , td [] [ text (toString away.yardsPerPass) ]
                    , td [] [ text (toString home.yardsPerPass) ]
                    ]
                , tr []
                    [ td [] [ text "Interceptions Thrown" ]
                    , td [] [ text (toString away.interceptionsThrown) ]
                    , td [] [ text (toString home.interceptionsThrown) ]
                    ]
                , tr []
                    [ td [] [ text "Total Sacks" ]
                    , td [] [ text (toString away.totalSacks) ]
                    , td [] [ text (toString home.totalSacks) ]
                    ]
                , tr []
                    [ td [] [ text "Yards Lost From Sacks" ]
                    , td [] [ text (toString away.yardsLostFromSacks) ]
                    , td [] [ text (toString home.yardsLostFromSacks) ]
                    ]
                , tr []
                    [ td [] [ text "Total Rushing Yards" ]
                    , td [] [ text (toString away.totalRushingYards) ]
                    , td [] [ text (toString home.totalRushingYards) ]
                    ]
                , tr []
                    [ td [] [ text "Rushing Attempts" ]
                    , td [] [ text (toString away.rushingAttempts) ]
                    , td [] [ text (toString home.rushingAttempts) ]
                    ]
                , tr []
                    [ td [] [ text "Yards Per Rush" ]
                    , td [] [ text (toString away.yardsPerRush) ]
                    , td [] [ text (toString home.yardsPerRush) ]
                    ]
                , tr []
                    [ td [] [ text "Red Zone Made" ]
                    , td [] [ text (toString away.redZoneMade) ]
                    , td [] [ text (toString home.redZoneMade) ]
                    ]
                , tr []
                    [ td [] [ text "Red Zone Attempts" ]
                    , td [] [ text (toString away.redZoneAttempts) ]
                    , td [] [ text (toString home.redZoneAttempts) ]
                    ]
                , tr []
                    [ td [] [ text "Red Zone Efficiency" ]
                    , td [] [ text (toString away.redZoneMadePct) ]
                    , td [] [ text (toString home.redZoneMadePct) ]
                    ]
                , tr []
                    [ td [] [ text "Total Penalties" ]
                    , td [] [ text (toString away.totalPenalties) ]
                    , td [] [ text (toString home.totalPenalties) ]
                    ]
                , tr []
                    [ td [] [ text "Yards Given From Penalties" ]
                    , td [] [ text (toString away.yardsGivenFromPenalties) ]
                    , td [] [ text (toString home.yardsGivenFromPenalties) ]
                    ]
                , tr []
                    [ td [] [ text "Turnovers" ]
                    , td [] [ text (toString away.turnovers) ]
                    , td [] [ text (toString home.turnovers) ]
                    ]
                , tr []
                    [ td [] [ text "Fumbles Lost" ]
                    , td [] [ text (toString away.fumblesLost) ]
                    , td [] [ text (toString home.fumblesLost) ]
                    ]
                , tr []
                    [ td [] [ text "Defense/Special Teams TD" ]
                    , td [] [ text (toString away.defenseSpecialTeamsTD) ]
                    , td [] [ text (toString home.defenseSpecialTeamsTD) ]
                    ]
                , tr []
                    [ td [] [ text "Time of Possession" ]
                    , td [] [ text (toString away.timeOfPossession) ]
                    , td [] [ text (toString home.timeOfPossession) ]
                    ]
                ]
            ]
        

displayGame : Game -> Html Msg
displayGame game =
    div [ class "row" ]
        [ div [ class "col-12" ] 
              [ displayTeamInfoAndScores game
              , printGameTable game
              ]
        ]

view : Model -> Html Msg
view model =
  div []
    [ div []
        [ input [ type_ "number", placeholder "Year", onInput YearInput ] [ text (toString model.year) ]
        , input [ type_ "number", placeholder "Week", onInput WeekInput ] [ text (toString model.week) ]
        , viewValidateGetGames model
        ]
    , div []
        [ input [ type_ "text", placeholder "Game Id", onInput GameIdInput ] [ text model.gameId ]
        , viewValidateGetGame model
        ]
    , if model.showGame
      then displayGame model.game
      else div [] <| List.map displayGame model.games 
    ]

viewValidateGetGames : Model -> Html Msg
viewValidateGetGames model =
  let
      ( color, message, valid ) =
        if model.year > 2007 && (model.week > 0 && model.week <= 17) then
          ("green", "OK", False)
        else
          ("red", "Invalid year or week input.", True)
  in
    div [ style [("color", color)] ]
        [ text message
        , button [ onClick GetGames, disabled valid ] [ text "Get Games" ]
        ]

viewValidateGetGame : Model -> Html Msg
viewValidateGetGame model =
  let
      ( color, message, valid ) =
        if model.gameId /= "" then
          ("green", "OK", False)
        else
          ("red", "", True)
  in
    div [ style [("color", color)] ]
        [ text message
        , button [ onClick GetGame, disabled valid ] [ text "Get Game" ]
        ]