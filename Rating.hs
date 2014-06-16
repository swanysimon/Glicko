----
----
-- Rating.hs
-- Haskell program implementing the Glicko rating system
-- this module contains only computational functions and
--      the data types used to store information
-- can be run as a standalone program but meant to be
--      run with an html scraper of some kind to automate
--      the task of gathering and storing data and
--      calculating Glicko ratings
-- created by Simon Swanson
----
----


-- all functions and the data type that might be helpful if paired with another file
module Rating where

import Data.Char
import Data.List
import System.Environment
import System.IO


-- holds team name, current rating and deviation, number periods since last competition, and results of each match from the current rating period
data Glicko = Glicko {
    name            :: String,
    rating          :: Double,
    deviation       :: Double,
    time            :: Double,
    results         :: [Game]
} deriving (Show, Read)

data Game = Game {
    win             :: Double,
    number          :: Int,
    opprating       :: Double,
    oppdeviation    :: Double
} deriving (Show, Read)

instance Eq Glicko where
    g1 == g2 = name g1 == name g2

instance Ord Glicko where
    g1 `compare` g2 = (rating g1) `compare` (rating g2)

instance Eq Game where
    g1 == g2 = number g1 == number g2

instance Ord Game where
    g1 `compare` g2 = number g1 `compare` number g2


-- decay speed of ratings (lower = faster) for each sport
decay :: String -> Double
decay "mlb" = 54.77
decay "nba" = 59.41
decay _     = 0


-- rating period for each sport in days
-- Glicko states that each rating period should try for 5-10 games
period :: String -> Integer
period "mlb" = 8
period "nba" = 14
period _     = 0


-- updates game result information for each team, making no game is entered twice
-- [[[String]]] is of the form [[[team name, game number, game result],[team name, game number, game result]]]
updateGlicko :: [[[String]]] -> [Glicko] -> [Glicko]
updateGlicko [[[]]] gs = gs
updateGlicko (x:xs) gs
    | newGame `elem` (results team1) = updateGlicko xs gs
    | otherwise = (updateGlicko xs . updateList (makeTeam team2 win2 game2 team1) . updateList (makeTeam team1 win1 game1 team2)) gs where
        [name1, game1, win1] = head x
        [name2, game2, win2] = last x
        team1 = findTeam name1 gs
        team2 = findTeam name2 gs
        newGame = Game {win = read win1 :: Double, number = read game1 :: Int, opprating = rating team2, oppdeviation = deviation team2}
        makeTeam r1 w g r2 = r1 {results = Game {win = read w :: Double, number = read g :: Int, opprating = rating r2, oppdeviation = deviation r2} : results r1}


-- retrieves the new rating for a team, given the sport name and its Glicko data, rounding to the nearest integer
runGlicko :: String -> Glicko -> Glicko
runGlicko league team = team {
    rating      = (fromIntegral . round . newRating) updatedDev,
    deviation   = (fromIntegral . round . newdeviation) updatedDev,
    results     = [],
    time        = case results team of
        [] -> time team + 1
        _ -> 1
    } where
        updatedDev = team {deviation = minimum [350, ((deviation team) ** 2 + time team * (decay league) ** 2) ** (-1)]}


-- the default settings for a new team
newTeam ::  String -> Glicko
newTeam teamname = Glicko {name = teamname, rating = 1500, deviation = 350, time = 1, results = []}


-- if the team is not in a list yet, adds it with the default rating
newList :: String -> [Glicko] -> [Glicko]
newList teamname [] = [newTeam teamname]
newList teamname (g:gs)
    | name g == teamname = (g:gs)
    | otherwise = g : newList teamname gs


-- finds the team in a list and gets its information
findTeam :: String -> [Glicko] -> Glicko
findTeam s [] = newTeam s
findTeam s (x:xs)
    | name x == s = x
    | otherwise = findTeam s xs


-- updates the list of teams, replacing the old ranking for a team with a new one
updateList :: Glicko -> [Glicko] -> [Glicko]
updateList g gs = a ++ g : y where
    (x,y) = splitAt index gs
    a = take (length x - 1) x
    index = case elemIndex g gs of
        Just a -> a + 1
        Nothing -> 0


-- calculates the new rating for a team based on the Glicko-1 specifications
-- personal minimum set at 100
newRating :: Glicko -> Double
newRating team = maximum [100, rating team + 0.0057565 / (1 / (deviation team) ** 2 + 1 / dSquared team) * foldr (ratingSum team) 0 (results team)]


-- with foldr takes care of the summation function necessary for newRating
ratingSum :: Glicko -> Game -> Double -> Double
ratingSum team game value = gRD game * (win game - eSR team game) + value


dSquared :: Glicko -> Double
dSquared team = (0.0057565 ** 2 * foldr (squareSum team) 0 (results team)) ** (-1)


-- with foldr takes care of the summation function necessary for dSquared
squareSum :: Glicko -> Game -> Double -> Double
squareSum team game value = gRD game ** 2 * eSR team game * (1 - eSR team game) + value


newdeviation :: Glicko -> Double
newdeviation team = minimum [350, maximum [30, (1 / (deviation team) ** 2 + 1 / dSquared team) ** (-1/2)]]


-- computational functions involved in the Glicko-1 specification
eSR :: Glicko -> Game -> Double
eSR team game = (1 + 10 ** (gRD game * (rating team - opprating game) / (-400))) ** (-1)

gRD :: Game -> Double
gRD game = sqrt (1 + 3 * (0.0057565 * oppdeviation game / pi) ** 2) ** (-1)
