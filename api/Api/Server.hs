module Api.Server
    ( server
    ) where

import Control.Concurrent.STM
       (TVar, atomically, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Data.Either
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.UUID (toString)
import Network.HTTP.Client
import Servant ((:<|>)((:<|>)), Server, Handler)
import System.Random (randomIO)

import Api.Lib
import Api.Types (Api, Game)

server :: Server Api
server = gamesHandler
  where
    gamesHandler :: Maybe String -> Maybe String -> Handler [Game]
    gamesHandler year week = do
        let queryYear =
                case year of
                    Just value -> value
                    Nothing -> "noyear"
            queryWeek =
                case week of
                    Just value -> value
                    Nothing -> "week"
            url = buildWeekUrl queryYear queryWeek
        manager <- liftIO $ newManager defaultManagerSettings
        gameIds <- liftIO $ getGameIds =<< (makeRequest manager url)
        games <- liftIO $ getGames manager gameIds
        let partitionedGames = partitionEithers games
        return $ (snd partitionedGames)
