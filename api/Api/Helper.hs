module Api.Helper
    ( string2Int
    , string2Double
    ) where

string2Int :: String -> Int
string2Int str = read str :: Int

string2Double :: String -> Double
string2Double str = read str :: Double
