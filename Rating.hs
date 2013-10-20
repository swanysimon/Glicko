----
----
-- Rating.hs
-- Haskell program implementing the Glicko rating system
-- contains only computational functions and
--      the data type used to store information
-- can be run as a standalone program but meant to be
--      run with an html scraper of some kind to automate
--      the task of gathering and storing data
-- created by Simon Swanson
----
----


-- all functions and the data type that might be helpful if paired with another file
module Rating (Glicko(..), tup1, tup2, tup3, upList, newList, findTeam, runGlicko, c, n, updateGlicko) where

import Data.Char
import Data.List
import System.Environment
import System.IO


-- holds team name, current rating and deviation, time since last competition, and results from the current rating period
-- results holds w/l/d from each game, game number (to avoid double-counting games), and the rating and deviation of the opponent
data Glicko = Glicko {
    team :: String,
    rating :: Double,
    dev :: Double,
    time :: Double,
    results :: [(String, Int, [Double])]
} deriving (Show, Read)

-- teams should be distinguished by their name. makes life so much easier when using upList
-- probably bad to compare different parts of the data type for Ord and Eq, but it works for my simple purposes here
instance Eq Glicko where
    g1 == g2 = team g1 == team g2

-- only used while sorting, so I think it's acceptable to make a different instance for this and EQ
instance Ord Glicko where
    g1 `compare` g2 = (rating g1) `compare` (rating g2)



-- functions to get the parts of a three tuple I need (useful when extracting info from result)
tup1 :: (a, b, c) -> a
tup1 (a, _, _) = a

tup2 :: (a, b, c) -> b
tup2 (_, b, _) = b

tup3 :: (a, b, c) -> c
tup3 (_, _, c) = c


-- helper functions to get constant values
-- c gets the decay speed of the ratings (lower = faster)
c :: String -> Double
c s = case map toLower s of
    "mlb" -> 54.77
    "nba" ->59.41
    "d" -> 0.5
    "q" -> 0.0057565
    "w" -> 1
    _ -> 0

-- n gets the rating period time for each sport in days
n :: String -> Integer
n s = case map toLower s of
    "nba" -> 15
    "mlb" -> 10
    _ -> 0



-- the default ranking for a new team
newTeam ::  String -> Glicko
newTeam s = Glicko {team = s, rating = 1500, dev = 350, time = 1, results = []}


-- if the team is not in a list yet, adds it with the default rating
newList :: [Glicko] -> String -> [Glicko] -> [Glicko]
newList xs s [] = newTeam s : xs
newList xs s (g:gs)
    | team g == s = xs
    | otherwise = newList xs s gs


-- finds the team in a list and gets its information
findTeam :: [Glicko] -> String -> Glicko
findTeam [] s = newTeam s
findTeam (x:xs) s
    | team x == s = x
    | otherwise = findTeam xs s


-- updates the list of teams, replacing the old ranking for a team with a new one
upList :: Glicko -> [Glicko] -> [Glicko]
upList g gs = (take (length x - 1) x) ++ g : y where
    (x,y) = splitAt index gs
    index = case elemIndex g gs of
        Just a -> a + 1
        Nothing -> 0


-- updates game result information for each team. ugly function but entirely necessary, unfortunately
updateGlicko :: [[[String]]] -> [Glicko] -> [Glicko]
updateGlicko [] gs = gs
updateGlicko [[]] gs = gs
updateGlicko [[[]]] gs = gs
updateGlicko (x:xs) gs
    -- check to make sure no game is entered twice
    | (w1, g1, [rating t2, dev t2]) `elem` results t1 = updateGlicko xs gs
    | otherwise = updateGlicko xs $ upList (mkTeam t1 w1 g1 t2) $ upList (mkTeam t2 w2 g2 t1) gs where
        t1 = (findTeam gs . head . head) x
        t2 = (findTeam gs . head . last) x
        w1 = (last . head) x
        w2 = (last . last) x
        g1 = read ((head x) !! 1) :: Int
        g2 = read ((last x) !! 1) :: Int
        mkTeam r1 w g r2 = r1 {results = (w, g, [rating r2, dev r2]) : results r1}


-- retrieves the new rating for a team, given the sport name and its Glicko data, rounding to the nearest integer
runGlicko :: String -> Glicko -> Glicko
runGlicko s g = Glicko {team = team g, rating = rnd $ newRating devg, dev = rnd $ newDev devg, time = time devg, results = []} where
    rnd = fromIntegral . round
    devg = g {
        dev = minimum [350, sqrt ((dev g) ^ 2 + time g * (c s) ^ 2)],
        time = case results g of
            [] -> time g + 1
            _ -> 1
        }

-- calculates the new rating for a team based on the Glicko-1 specifications, making sure it never drops below 100
newRating :: Glicko -> Double
newRating g = maximum [100, rating g + c "q" / (1 / (dev g) ^ 2 + 1 / dSquared g) * summation (results g)] where
    summation xs
        | null xs = 0
        | otherwise = gRD (last $ tup3 $ head xs) * (c (filter isAlpha $ tup1 $ head xs) - eSR g (tup3 $ head xs)) + summation (tail xs)


-- calculates the new deviation for a team based on the Glicko-1 specifications, making sure it stays between 30 and 350
newDev :: Glicko -> Double
newDev g = minimum [350, maximum [30, sqrt (1 / (dev g) ^ 2 + 1 / dSquared g) ** (-1)]]


-- the essential computational functions involved in the Glicko-1 specification
eSR :: Glicko -> [Double] -> Double
eSR g r = (1 + 10 ** (gRD (last r) * (rating g - head r) / (-400))) ** (-1)

gRD :: Double -> Double
gRD x = sqrt (1 + 3 * (c "q" * x / pi) ^ 2) ** (-1)

dSquared :: Glicko -> Double
dSquared g = ((c "q") ^ 2 * summation (results g)) ** (-1) where
    summation xs
        | null xs = 0
        | otherwise = gRD (last $ tup3 $ head xs) ^ 2 * eSR g (tup3 $ head xs) * (1 - eSR g (tup3 $ head xs)) + summation (tail xs)
