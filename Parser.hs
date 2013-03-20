-- Parser.hs
-- Simon Swanson
-- takes in the HTML files and parses, writing out to file
-- currently only works for basketball

module Main where

import Rating
import System.IO
import System.Environment (getArgs, getProgName)
import Network.HTTP (simpleHTTP, getResponseBody, getRequest)
import Text.HTML.TagSoup
import Data.List
import Data.Char
import Text.Regex.Posix
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (diffDays, addDays, fromGregorian, toGregorian, Day)
import Data.Text (split, pack, unpack)


-- because of the way ~== and =~ operate I have to create my own dropWhile function
scoreDrop :: [Tag String] -> [Tag String]
scoreDrop xs | null xs = []
    | head xs ~== TagOpen "div" [] && fromAttrib "id" (head xs) =~ "scores-[0-9]+" = xs
    | otherwise = scoreDrop (tail xs)


-- retrieves games that have gone final
final :: [[Tag String]] -> [[Tag String]]
final xs | null xs = []
    | elem (TagOpen "td" [("class","finalscore"),("align","center")]) (head xs) && elem (TagOpen "font" [("class","winarrow")]) (head xs) = head xs : final (tail xs)
    | otherwise = final (tail xs)


-- breaks up the information until all that is left is the team name, its record, and, if it won, an indicator saying it won
segments :: String -> [[[String]]]
segments = text . isolate . extras . map (splitTeam) . final . parts . ftaken . scoreDrop . parseTags where
    ftaken = takeWhile (/= TagOpen "div" [("class", "scoreboardKey")])
    parts = partitions (~== TagOpen "table" [("class", "lineScore")])
    splitTeam = partitions (~== TagOpen "tr" [("id", "final"), ("align", "right")])
    extras xs | null xs = []
        | otherwise = map (takeWhile (/= TagOpen "td" [("align","center")])) (head xs) : extras (tail xs)
    isolate xs | null xs =[]
        | otherwise = map (filter isTagText) (head xs) : isolate (tail xs)
    text xs | null xs = []
        | otherwise = map (map fromTagText) (head xs) : text (tail xs)


-- takes in a teams record and turns it into the number of played games
gameEval :: String -> Int
gameEval = sum . makenum . map unpack . split (== '-') . pack . filter simDigit where
    makenum x = map read x :: [Int]
    simDigit n = (n >= '0' && n <= '9') || (n == '-')


-- cleans up the lists to prepare for the final execution, with each team now represented by [name, gamesplayed, gameresult]
clear :: [[String]] -> [[String]]
clear xs | null xs = []
    | xs == [[]] = []
    | (length $ head xs) == 3 = [[(head . head) xs, (show . gameEval) ((head xs) !! 1), "w"], [(head . last) xs, (show . gameEval) ((last xs) !! 1), "l"]]
    | (length $ last xs) == 3 = [[(head . head) xs, (show . gameEval) ((head xs) !! 1), "l"], [(head . last) xs, (show . gameEval) ((last xs) !! 1), "w"]]
    | otherwise = [[(head . head) xs, (show . gameEval) ((head xs) !! 1), "d"], [(head . last) xs, (show . gameEval) ((last xs) !! 1), "d"]]


getTeams :: [Glicko] -> [[[String]]] -> [Glicko]
getTeams gs xs | null xs = gs
    | xs == [[]] = gs
    | xs == [[[]]] = gs
    | otherwise = getTeams (newList rs (head $ last $ head xs) rs) (tail xs) where
        rs = newList gs (head $ head $ head xs) gs


-- updates the Glicko for each team in a game
updateGlicko :: [[[String]]] -> [Glicko] -> [Glicko]
updateGlicko xs gs | null xs = gs
    | xs == [[]] = []
    | xs == [[[]]] = []
    | tup2 ((head . res . glicko1 . head) xs) == games head (head xs) = updateGlicko (tail xs) gs
    | otherwise = updateGlicko (tail xs) (new (head xs)) where
        glicko1 = findTeam gs . head . head
        glicko2 = findTeam gs . head . last
        games f ns = read (((f) ns) !! 1) :: Int
        team1 ns = Glicko {
            team = team $ glicko1 ns,
            rating = rating $ glicko1 ns,
            dev = dev $ glicko1 ns,
            time = time $ glicko1 ns,
            res = (last $ head ns, games head ns, [rating $ glicko2 ns, dev $ glicko2 ns]) : (res $ glicko1 ns)
        }
        team2 ns = Glicko {
            team = head $ last ns,
            rating = rating $ glicko2 ns,
            dev = dev $ glicko2 ns,
            time = time $ glicko2 ns,
            res = (last $ last ns, games last ns, [rating $ glicko1 ns, dev $ glicko1 ns]) : (res $ glicko2 ns)
        }
        new rs = upList (team2 rs) (upList (team1 rs) gs)


-- takes in the current date, the starting date, the league name, all the teams for that league, and the day for evaluating
-- writes all undocumented games to file, updating ratings as necessary
checkEval :: Day -> Day -> String -> [Glicko] -> Day -> IO ()
checkEval curDate oldDate league teams eval = case diffDays oldDate eval of
    -- if it's the day after the designated evaluation date (to make sure every game finishes), evaluates before adding in any new games
        1 -> do
            putStrLn $ "Ratings updated as of " ++ (show eval) ++"."
            run curDate oldDate league (map (runGlicko league) teams) (addDays (n league) eval)
        _ -> run curDate oldDate league teams eval


run :: Day -> Day -> String -> [Glicko] -> Day -> IO ()
run curDate oldDate league teams eval = do
    putStrLn $ "Processing games from " ++ (show oldDate) ++ "."
    http <- simpleHTTP (getRequest $ "http://www.cbssports.com/" ++ league ++ "/scoreboard/" ++ filter (/= '-') (show oldDate)) >>= getResponseBody
    let fscores = map clear $ segments http
    case fscores of
        [] -> putStrLn "Done."
        _ -> do
            let newteams = (reverse . sort . updateGlicko fscores) (getTeams teams fscores)
            case diffDays curDate oldDate of
                
                0 -> do
                    let path = "/Users/Swanson/Programs/Haskell/glicko-ratings/glicko" ++ league
                    writeFile path $ (show . toGregorian) curDate ++ "\n" ++ show newteams ++ "\n" ++ (show . toGregorian) eval
                    putStrLn $ "Current " ++ league ++ " rankings:"
                    prettyprint newteams where
                        prettyprint xs | null xs = return ()
                            | otherwise = do
                                putStrLn $ (team . head) xs
                                prettyprint (tail xs)

                _ -> checkEval curDate (addDays 1 oldDate) league newteams eval


main :: IO ()
main = do
    args <- getArgs
    case length args of

        1 -> do
            curDate <- getCurrentTime >>= return . utctDay
            let league = map toLower (head args)
            fp <- readFile ("/Users/Swanson/Programs/Haskell/glicko-ratings/glicko" ++ league) >>= return . lines
            let e = read $ head fp :: (Integer, Int, Int)
            let oldDate = fromGregorian (tup1 e) (tup2 e) (tup3 e)
            let d = read (fp !! 2) :: (Integer, Int, Int)
            let eval = fromGregorian (tup1 d) (tup2 d) (tup3 d)
            let teams = read (fp !! 1) :: [Glicko]
            checkEval curDate oldDate league teams eval

        _ -> do
            prog <- getProgName
            putStrLn $ "Usage: " ++ (show prog) ++ " leaguename"

