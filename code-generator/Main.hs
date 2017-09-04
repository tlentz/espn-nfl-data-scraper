{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds         #-}

module Main where
import Data.Proxy (Proxy (Proxy))
import Elm (Spec (Spec), specsToDir, toElmTypeSource, toElmDecoderSource, toElmEncoderSource)
import Servant.Elm (ElmOptions (..), defElmImports, defElmOptions, generateElmForAPIWith, UrlPrefix (Static))
import Api.Types
import Data.Text as DT
import Api.Example.Types (Dice)
import Api.NFL.Types (Game, Team, TeamInfo, GameStats, Scores)

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "/espn-nfl-web-scraper" }
  
elmImports :: [Text]
elmImports = [ "import Dict exposing (Dict)"
             ]


specs :: [Spec]
specs =
  [ 
    Spec ["Shared", "Generated"]
         ( defElmImports `append` (DT.intercalate "\n" elmImports)
         : toElmTypeSource (Proxy :: Proxy Dice)
         : toElmDecoderSource (Proxy :: Proxy Dice)
         : toElmEncoderSource (Proxy :: Proxy Dice)
         : toElmTypeSource (Proxy :: Proxy Game)
         : toElmDecoderSource (Proxy :: Proxy Game)
         : toElmEncoderSource (Proxy :: Proxy Game)
         : toElmTypeSource (Proxy :: Proxy Team)
         : toElmDecoderSource (Proxy :: Proxy Team)
         : toElmEncoderSource (Proxy :: Proxy Team)
         : toElmTypeSource (Proxy :: Proxy TeamInfo)
         : toElmDecoderSource (Proxy :: Proxy TeamInfo)
         : toElmEncoderSource (Proxy :: Proxy TeamInfo)
         : toElmTypeSource (Proxy :: Proxy GameStats)
         : toElmDecoderSource (Proxy :: Proxy GameStats)
         : toElmEncoderSource (Proxy :: Proxy GameStats)
         : toElmTypeSource (Proxy :: Proxy Scores)
         : toElmDecoderSource (Proxy :: Proxy Scores)
         : toElmEncoderSource (Proxy :: Proxy Scores)
         : generateElmForAPIWith elmOpts  (Proxy :: Proxy Api)
         )
  ]

main :: IO ()
main = specsToDir specs "frontend/elm"