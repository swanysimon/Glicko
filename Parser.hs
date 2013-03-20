-- Parser.hs
-- Simon Swanson
-- takes in the HTML files and parses, writing out to file
-- currently only works for basketball and baseball (be careful with Spring Training)

module Main where

import Rating
import System.IO
import System.IO.Strict as Strict
import System.Environment
import Network.HTTP
import Text.HTML.TagSoup
import Data.List
import Data.Char
import Text.Regex.Posix
import Data.Time.Clock
import Data.Time.Calendar
import Data.Text (split, pack, unpack)


-- because of the way ~== and =~ operate I have to create my own dropWhile function
scoreDrop :: [Tag String] -> [Tag String]
scoreDrop xs | null xs = []
    | head xs ~== TagOpen "div" [] && fromAttrib "class" (head xs) =~ "spanCol[0-9]?" = xs
    | otherwise = scoreDrop (tail xs)

-- roughly the end of the scoreboard for both mlb and nba
taken :: [Tag String] -> [Tag String]
taken xs | null xs = []
    | head xs ~== TagOpen "div" [] && fromAttrib "class" (head xs) =~ "score[a-zA-z]*Key" = []
    | otherwise = (head xs) : taken (tail xs)


-- retrieves games that have gone final
final :: [[Tag String]] -> [[Tag String]]
final xs | null xs = []
    | elem (TagOpen "td" [("class","finalscore"),("align","center")]) (head xs) && elem (TagOpen "font" [("class","winarrow")]) (head xs) = head xs : final (tail xs)
    | otherwise = final (tail xs)


-- breaks up the information until all that is left is the team name, its record, and, if it won, an indicator saying it won
segments :: String -> [[[String]]]
segments = doubleheader . text . isolate . extras . map (splitTeam) . final . parts . taken . scoreDrop . parseTags where
    parts = partitions (~== TagOpen "table" [("class", "lineScore")])
    splitTeam = partitions (~== TagOpen "tr" [("id", "final"), ("align", "right")])
    extras xs | null xs = []
        | otherwise = map (takeWhile (/= TagClose "td")) (head xs) : extras (tail xs)
    isolate xs | null xs =[]
        | otherwise = map (filter isTagText) (head xs) : isolate (tail xs)
    text xs | null xs = []
        | otherwise = map (map fromTagText) (head xs) : text (tail xs)
    doubleheader xs | null xs = []
        | (head xs) `elem` (tail xs) = (map decrease (head xs)) : doubleheader (tail xs)
        | otherwise = (head xs) : doubleheader (tail xs)
    decrease xs = case xs of
        [] -> []
        _ -> (head xs ++ "1") : drop 1 xs


-- takes in a team's record and turns it into the number of played games
gameEval :: String -> String -> Int
gameEval team s | simfilter team == team = supersum s
    | otherwise = supersum s - 1 where
        supersum = sum . makenum . map unpack . split (== '-') . pack . filter simDigit
        makenum x = map read x :: [Int]
        simDigit n = (n >= '0' && n <= '9') || (n == '-')


-- cleans up the lists to prepare for the final execution, with each team now represented by [name, gamesplayed, gameresult]
clear :: [[String]] -> [[String]]
clear xs | null xs = []
    | xs == [[]] = []
    | (length $ head xs) == 3 = [[simfilter $ (head . head) xs, (show . gameEval (head (head xs))) ((head xs) !! 1), "w"], [simfilter $ (head . last) xs, (show . gameEval (head (head xs))) ((last xs) !! 1), "l"]]
    | (length $ last xs) == 3 = [[simfilter $ (head . head) xs, (show . gameEval (head (head xs))) ((head xs) !! 1), "l"], [simfilter $ (head . last) xs, (show . gameEval (head (head xs))) ((last xs) !! 1), "w"]]
    | otherwise = [[simfilter $ (head . head) xs, (show . gameEval (head (head xs))) ((head xs) !! 1), "d"], [simfilter $ (head . last) xs, (show . gameEval (head (head xs))) ((last xs) !! 1), "d"]]


simfilter :: String -> String
simfilter s | null s = ""
    | last s == '1' = init s
    | otherwise = s


getTeams :: [Glicko] -> [[[String]]] -> [Glicko]
getTeams gs xs | null xs = gs
    | xs == [[]] = gs
    | xs == [[[]]] = gs
    | otherwise = getTeams (newList rs (head $ last $ head xs) rs) (tail xs) where
        rs = newList gs (head $ head $ head xs) gs


-- takes in the current date, the starting date, the league name, all the teams for that league, and the day for evaluating
-- writes all undocumented games to file, updating ratings as necessary
checkEval :: Day -> Day -> String -> [Glicko] -> Day -> IO ()
checkEval curDate oldDate league teams eval = case diffDays oldDate eval of
    -- if it's the day after the designated evaluation date (to make sure every game finishes), evaluates before adding in any new games
        1 -> do
            putStrLn $ "Ratings updated as of " ++ (showGregorian eval) ++"."
            run curDate oldDate league (map (runGlicko league) teams) (addDays (n league) eval)
        _ -> run curDate oldDate league teams eval


run :: Day -> Day -> String -> [Glicko] -> Day -> IO ()
run curDate oldDate league teams eval = do
    putStrLn $ "Processing games from " ++ (showGregorian oldDate) ++ "."
    http <- simpleHTTP (getRequest $ "http://www.cbssports.com/" ++ league ++ "/scoreboard/" ++ filter (/= '-') (showGregorian oldDate)) >>= getResponseBody
    let fscores = map clear $ segments http

    case fscores of
        [] -> checkDate curDate oldDate league teams eval
        _ -> checkDate curDate oldDate league (updateGlicko fscores (getTeams teams fscores)) eval


checkDate :: Day -> Day -> String -> [Glicko] -> Day -> IO ()
checkDate curDate oldDate league teams eval = case curDate `compare` oldDate of
    EQ -> do
        let newCur = show $ toGregorian curDate
        let newEval = show $ toGregorian eval
        let newTeam = reverse $ sort $ teams
        writeFile ("/Users/Swanson/Programs/Haskell/glicko-ratings/glicko" ++ league) (newCur ++ "\n" ++ (show newTeam) ++ "\n" ++ newEval)
        prettyprint newTeam where
            prettyprint xs | null xs = return ()
                | otherwise = do
                    putStrLn $ show (rating (head xs)) ++ " - " ++ team (head xs)
                    prettyprint (tail xs)
    _ -> checkEval curDate (addDays 1 oldDate) league teams eval


main :: IO ()
main = do
    args <- getArgs
    case length args of

        1 -> do
            curDate <- getCurrentTime >>= return . utctDay
            let league = map toLower (head args)
            -- strict file reading because Haskell hates the world and I don't have time to figure out ByteStrings right now (which are apparently strict)
            fp <- Strict.readFile ("/Users/Swanson/Programs/Haskell/glicko-ratings/glicko" ++ league) >>= return . lines
            let e = read $ head fp :: (Integer, Int, Int)
            let oldDate = fromGregorian (tup1 e) (tup2 e) (tup3 e)
            let d = read (fp !! 2) :: (Integer, Int, Int)
            let eval = fromGregorian (tup1 d) (tup2 d) (tup3 d)
            let teams = read (fp !! 1) :: [Glicko]
            checkEval curDate oldDate league teams eval

        3 -> do
            let c = read (last args) :: (Integer, Int, Int)
            let curDate = fromGregorian (tup1 c) (tup2 c) (tup3 c)
            let g = read (args !! 1) :: (Integer, Int, Int)
            let oldDate = fromGregorian (tup1 g) (tup2 g) (tup3 g)
            let league = map toLower (head args)
            fp <- Strict.readFile ("/Users/Swanson/Programs/Haskell/glicko-ratings/glicko" ++ league) >>= return . lines
            let eval = addDays (n league) oldDate
            let teams = read (fp !! 1) :: [Glicko]
            putStrLn $ "Evaluating from " ++ (showGregorian oldDate) ++ " to " ++ (showGregorian curDate) ++ ". Understand that this may create problems in files and should be used only for testing purposes."
            checkEval curDate oldDate league teams eval       

        _ -> do
            prog <- getProgName
            putStrLn $ "Usage: " ++ (show prog) ++ " leaguename"
