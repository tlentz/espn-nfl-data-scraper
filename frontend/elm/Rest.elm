module Rest exposing (..)

import Shared.Generated exposing (..)
import Http
import Types exposing (..)

getGame : String -> Cmd Msg
getGame gameId =
    Http.send processGetGame <| getApiGetGameByGameId gameId

processGetGame : Result Http.Error Game -> Msg
processGetGame result =
    case result of
        Ok game ->
            RecieveGame game
        
        Err err ->
            Error err

getGames : Int -> Int -> Cmd Msg
getGames year week =
    Http.send processGetGames <| getApiGetGamesByYearByWeek year week

processGetGames : Result Http.Error (List Game) -> Msg
processGetGames result =
    case result of
        Ok games ->
            RecieveGames games
        
        Err err ->
            Error err