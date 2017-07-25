{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Proxy  (Proxy (Proxy))
import           Elm         (Spec (Spec), specsToDir, toElmTypeSource,
                              toElmDecoderSource, toElmEncoderSource)
import           Servant.Elm (ElmOptions (..), defElmImports, defElmOptions,
                              generateElmForAPIWith, UrlPrefix (Static))

import           Api.Types   (Api, Game, Team, TeamInfo, GameStats, Scores)

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "http://localhost:8000/api" }

specs :: [Spec]
specs =
  [ Spec ["Generated", "Api"]
         (defElmImports
          : toElmTypeSource    (Proxy :: Proxy Game)
          : toElmDecoderSource (Proxy :: Proxy Game)
          : toElmTypeSource    (Proxy :: Proxy Team)
          : toElmDecoderSource (Proxy :: Proxy Team)
          : toElmTypeSource    (Proxy :: Proxy TeamInfo)
          : toElmDecoderSource (Proxy :: Proxy TeamInfo)
          : toElmTypeSource    (Proxy :: Proxy GameStats)
          : toElmDecoderSource (Proxy :: Proxy GameStats)
          : toElmTypeSource    (Proxy :: Proxy Scores)
          : toElmDecoderSource (Proxy :: Proxy Scores)
          : generateElmForAPIWith elmOpts  (Proxy :: Proxy Api))
  ]

main :: IO ()
main = specsToDir specs "frontend/src"
