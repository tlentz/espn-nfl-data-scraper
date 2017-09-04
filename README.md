# espn-nfl-data-scraper

## Description
* Web scraper that scrapes NFL Game Data from ESPN using haskell/servant for the backend and Elm for the frontend

## Prerequisites
1. Install [NodeJS v6.9+](https://nodejs.org/en/download/current/)
2. Install [Elm v0.18](http://install.elm-lang.org/)
3. Install [Ruby](https://www.ruby-lang.org/en/documentation/installation/)
4. Install [Stack](https://docs.haskellstack.org/en/stable/README/)
5. Run stack setup `stack setup`
6. Install hpack `stack install hpack`
7. Install typescript globally `npm install -g typescript`
8. Install fusebox globally `npm install -g fuse-box`
9. Run `npm install`

## Running the Program
1. open a terminal in the root directory of the project
2. run `rake`
3. open your browser to http://localhost:3000/espn-nfl-data-scraper

## How it Works
* Input the year and week then click `Get Games` to scrape all the games for that year/week
* Input the espn game id then click `Get Game` to scrape a specific game
* This only works for NFL games
* If the game is not found or there is no data for that game, everything will be unknown/-1 when it returns
* Currently only works for up to year `2016`