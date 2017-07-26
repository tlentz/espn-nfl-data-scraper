module Main exposing (main)

import Html exposing (div, button, text, input, p, h1, h2, form, Html, ul, li)
import Html.Attributes exposing (placeholder, value, type_, class)
import Html.Events exposing (onClick, onInput)
import Http
import Result
import Json.Decode
import Generated.Api exposing (Game, decodeGame, Team, decodeTeam, TeamInfo, decodeTeamInfo, GameStats, decodeGameStats, Scores, decodeScores)


main : Program Never Model Msg
main =
  Html.program
    { init = (init, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     Sub.batch
--         [getGames RecieveGames]

type alias Model = 
    { games : List Game
    , year : String
    , week : String
    }

init : Model
init = {games = [], year = "", week = ""}

type Msg 
    = YearInput String
    | WeekInput String
    | GetGames
    | RecieveGames (List Game)
    | Error Http.Error


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        YearInput str -> ({model | year = str}, Cmd.none)
            
        WeekInput str -> ({model | week = str}, Cmd.none)
        
        GetGames -> (model, getGames model)

        RecieveGames games -> 
            let
                temp = Debug.log "got stuff back" "asdf"
            in
                ({model | games = games}, Cmd.none)

        Error err ->  ({model | year = "error"}, Cmd.none)


getGames : Model -> Cmd Msg
getGames model =    
    Http.send processGames <| Http.get ("/api/games?year=" ++ model.year ++ "&week="++ model.week) decodeGames

decodeGames : Json.Decode.Decoder (List Game)
decodeGames = 
    Json.Decode.list decodeGame

processGames : Result Http.Error (List Game) -> Msg
processGames result =
    let
        temp = Debug.log "process" result
    in
        case result of
            Ok games ->
                RecieveGames games

            Err err ->
                Error err



scores : Scores -> Html Msg
scores sc =
    div [] [text <| "Q1:" ++ (toString sc.q1) ++ " Q2:"++ (toString sc.q2) ++ " Q3:"++(toString sc.q3) ++ " Q4:"++ (toString sc.q4) ++ " Final:" ++ (toString sc.final)]

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

teamInfo : TeamInfo -> Html Msg
teamInfo info =
    let
        whereAt = if info.isHome then li [] [text "Home"] else li [] [text "Away"]
    in
        div []
            [ ul []
                [ li [] [text <| "Name: " ++ info.longName]
                , whereAt
                ]
            ]

displayTeam : Team -> Html Msg
displayTeam team =
    div []
        [ ul [] 
            [ li [] [teamInfo team.info]
            , li [] [stats team.stats]
            , li [] [scores team.scores]
            ]
        ]

displayGame : Game -> Html Msg
displayGame game =
    div [class "row"]
        [ div [class "col-6"] [ displayTeam <| Tuple.first game.teams ]
        , div [class "col-6"] [ displayTeam <| Tuple.second game.teams ]
        ]

view : Model -> Html Msg
view model =
    let
        temp = Debug.log "view" model
    in
        div [class "container form-group"]
            [ div [ class "form-control" ] [text "Year", input [onInput YearInput] []]
            , div [ class "form-control" ] [text "Week", input [onInput WeekInput] []]
            , button [ onClick GetGames, class "btn btn-primary" ] [text "Get Games"]
            , if (List.length model.games) > 0 then div [] <| List.map displayGame model.games else div [] [text "noGames"]
            ]