-- Rating.hs
-- Simon Swanson
-- mostly helper functions

module Rating (Glicko(..), tup1, tup2, tup3, upList, newList, findTeam, runGlicko, c, n, updateGlicko) where

import System.Environment
import System.IO
import Data.Char
import Data.List

-- stores the team name, rating, deviation, time since last competition, and results from the previous rating period
data Glicko = Glicko {
    team :: String,
    rating :: Double,
    dev :: Double,
    time :: Double,
    -- for each game, res should hold the result of the game, the game number for the team, and a list with the opponent's rating and deviation
    res :: [(String, Int, [Double])]
}
    deriving (Show, Read)

-- for comparing two teams I just use ratings. only used while sorting
instance Ord Glicko where
    g1 `compare` g2 = (rating g1) `compare` (rating g2)

-- teams should be distinguished by their name. makes life so much easier when using upList. bad Haskell to compare different parts of the data type for Ord and Eq? Maybe. But it works so I'm happy
instance Eq Glicko where
    g1 == g2 = team g1 == team g2


-- functions to get the parts of a three tuple I need
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
newTeam s = Glicko {team = s, rating = 1500, dev = 350, time = 1, res = []}


-- if the team is not in a list yet, adds it with a minimum ranking
newList :: [Glicko] -> String -> [Glicko] -> [Glicko]
newList xs s rs | null rs = newTeam s : xs
    | otherwise = if (team $ head rs) == s then xs else newList xs s (tail rs)


-- finds the team in a list and gets its information. should never hit null but hey, ya never know
findTeam :: [Glicko] -> String -> Glicko
findTeam xs s | null xs = newTeam s
    | otherwise = if (team $ head xs) == s then head xs else findTeam (tail xs) s


-- gets an updated list, replacing the old ranking for the team with the new one
upList :: Glicko -> [Glicko] -> [Glicko]
upList g gs = (take (length x - 1) x) ++ g : y where
    (x,y) = splitAt ind gs
    ind = case elemIndex g gs of
        Just a -> a + 1
        Nothing -> 0


-- updates the Glicko for each team using the game statistics. ugly function but entirely necessary
updateGlicko :: [[[String]]] -> [Glicko] -> [Glicko]
updateGlicko xs gs | null xs = gs
    | xs == [[]] = gs
    | xs == [[[]]] = gs
    | (w1, g1, [rating t2, dev t2]) `elem` res t1 = updateGlicko (tail xs) gs
    | otherwise = updateGlicko (tail xs) (upList (mkTeam t1 w1 g1 t2) (upList (mkTeam t2 w2 g2 t1) gs)) where
        t1 = (findTeam gs . head . head . head) xs
        t2 = (findTeam gs . head . last . head) xs
        w1 = (last . head . head) xs
        w2 = (last . last . head) xs
        g1 = read ((head $ head xs) !! 1) :: Int
        g2 = read ((last $ head xs) !! 1) :: Int
        mkTeam r1 w g r2 = r1 {res = (w, g, [rating r2, dev r2]) : res r1}


-- retrieves the new rating for a team, given the sport name and its Glicko data
runGlicko :: String -> Glicko -> Glicko
runGlicko s g = Glicko {team = team g, rating = rnd $ newRating devg, dev = rnd $ newDev devg, time = time devg, res = []} where
    rnd = fromIntegral . round
    devg = g {
        dev = minimum [350, sqrt ((dev g) ^ 2 + time g * (c s) ^ 2)],
        time = case res g of
            [] -> time g + 1
            _ -> 1
        }

-- calculates the new rating for a team, making sure it never drops below 100
newRating :: Glicko -> Double
newRating g = maximum [100, rating g + c "q" / (1 / (dev g) ^ 2 + 1 / dSquared g) * summation (res g)] where
    summation xs | null xs = 0
        | otherwise = gRD (last $ tup3 $ head xs) * (c (filter isAlpha $ tup1 $ head xs) - eSR g (tup3 $ head xs)) + summation (tail xs)


-- calculates the new deviation for a team, making sure it never drops below 30 and isn't greater than 350
newDev :: Glicko -> Double
newDev g = minimum [350, maximum [30, sqrt (1 / (dev g) ^ 2 + 1 / dSquared g) ** (-1)]]


-- the essential computational functions
eSR :: Glicko -> [Double] -> Double
eSR g r = (1 + 10 ** (gRD (last r) * (rating g - head r) / (-400))) ** (-1)

gRD :: Double -> Double
gRD x = sqrt (1 + 3 * (c "q" * x / pi) ^ 2) ** (-1)

dSquared :: Glicko -> Double
dSquared g = ((c "q") ^ 2 * summation (res g)) ** (-1) where
    summation xs | null xs = 0
        | otherwise = gRD (last $ tup3 $ head xs) ^ 2 * eSR g (tup3 $ head xs) * (1 - eSR g (tup3 $ head xs)) + summation (tail xs)
