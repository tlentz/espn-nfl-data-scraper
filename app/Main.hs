module Main where

import System.Environment (getArgs)

import Data.Either
import Lib
import Network.HTTP.Client
import Text.Pretty.Simple

main :: IO ()
main = do
    params <- getArgs
    let year = params !! 0
        week = params !! 1
        url = buildWeekUrl year week
    manager <- newManager defaultManagerSettings
    gameIds <- getGameIds =<< (makeRequest manager url)
    games <- getGames manager gameIds
    mapM_ (\g -> case g of
                    Left msg -> pPrint msg
                    Right game -> pPrint game) games