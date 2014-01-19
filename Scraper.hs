----
----
-- Scraper.hs
-- Haskell program implementing the Glicko rating system
-- contains only computational functions and
--      the data type used to store information
-- can be run as a standalone program but meant to be
--      run with an html scraper of some kind to automate
--      the task of gathering and storing data
-- created by Simon Swanson
----
----


module Main where

-- Scraper.hs must be in the same directory as Rating.hs, as it provides most of the computational functions
import Rating

import Control.Monad
import qualified Data.ByteString.Char8 as BS (readFile, unpack)
import Data.Char
import Data.List
import Data.Text (pack, unpack, split)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Maybe
import Network.HTTP
import System.Console.GetOpt
import System.Environment
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.IO
import Text.HTML.TagSoup
import Text.Regex.Posix


-- if it is the day after the evaluation date (to ensure all games have finished), does so before processing the current day's games
checkEval :: Day -> Day -> String -> [Glicko] -> Day -> IO ()
checkEval curDate oldDate league teams eval = case diffDays oldDate eval of
    1 -> do
        let updatedTeams = map (runGlicko league) teams
        let updatedEval = addDays (n league) eval
        putStrLn $ "Ratings updated as of " ++ (showGregorian eval) ++ "."
        scrape curDate oldDate league updatedTeams updatedEval
    _ -> scrape curDate oldDate league teams eval


-- the main function: this is where game information is parsed from the CBS website
scrape :: Day -> Day -> String -> [Glicko] -> Day -> IO ()
scrape curDate oldDate league teams eval = do
    putStrLn $ "Processing games from " ++ (showGregorian oldDate) ++ "."
    let date = filter (/= '-') (showGregorian oldDate)
    let url = "http://www.cbssports.com/" ++ league ++ "/scoreboard/" ++ date
    http <- simpleHTTP (getRequest url) >>= getResponseBody

    -- parses game results from the html and stores the information with the team
    let fscores = map getCleanResult $ segments http
    let updatedTeams = updateGlicko fscores $ getTeams teams fscores
    checkDate curDate oldDate league updatedTeams eval


-- checks if it is time to stop evaluation and write information out to file, as given by curDate
-- team order in output is sorted from best to worst rating
checkDate :: Day -> Day -> String -> [Glicko] -> Day -> IO ()
checkDate curDate oldDate league teams eval = case curDate `compare` oldDate of
    EQ -> do
        let newDate = show $ toGregorian curDate
        let newEval = show $ toGregorian eval
        let newTeam = show $ reverse $ sort $ teams
        let output = newDate ++ "\n" ++ newTeam ++ "\n" ++ newEval
        writeFile ("/Users/Swanson/Programs/Haskell/glicko-ratings/glicko" ++ league) output
    _ -> checkEval curDate (addDays 1 oldDate) league teams eval


-- makes sure all teams exist before entering information for them
getTeams :: [Glicko] -> [[[String]]] -> [Glicko]
getTeams gs [] = gs
getTeams gs [[]] = gs
getTeams gs [[[]]] = gs
getTeams gs (x:xs) = getTeams (newList rs (head $ last x) rs) xs where
        rs = newList gs (head $ head x) gs


-- cleans up the lists of game results and outputs each result in the form [opponent name, game number, game result]
getCleanResult :: [[String]] -> [[String]]
getCleanResult [] = []
getCleanResult [[]] = [[]]
getCleanResult [xs,ys]
    -- a length of 3 indicates that a win arrow for that team
    | length xs == 3 = [evalGame xs ++ ["w"], evalGame ys ++ ["l"]]
    | length ys == 3 = [evalGame xs ++ ["l"], evalGame ys ++ ["w"]]
    | otherwise = [evalGame xs ++ ["d"], evalGame ys ++ ["d"]]


-- helper function to get the team names for a game into correct order for processing
evalGame :: [String] -> [String]
evalGame (x:y:xs) = [dhFilter x, show $ gameEval x y]


-- removes the '1' added on by the doubleheader adjustment, if it exists
dhFilter :: String -> String
dhFilter "" = ""
dhFilter s
    | last s == '1' = init s
    | otherwise = s


-- takes in a team's record and turns it into the number of played games, making sure separate games of a doubleheader have different numbers
gameEval :: String -> String -> Int
gameEval team s
    | dhFilter team == team = supersum s
    | otherwise = supersum s - 1


-- takes in a string of the form 1-2-..-x and adds all the integer parts together
supersum :: String -> Int
supersum = sum . makenum . map unpack . split (== '-') . pack . filter simDigit where
    simDigit n = (n >= '0' && n <= '9') || (n == '-')
    makenum x = map read x :: [Int]


-- breaks up the html until all that is left is the team name, its record, and, if it won, an indicator saying it won
segments :: String -> [[[String]]]
segments = doubleheader . produceText . hasText . trimTags . gameResult . scoreTake . scoreDrop . parseTags


-- because of the way ~== and =~ operate I have to create my own versions of dropWhile and takeWhile to isolate the CBS scoreboard
-- this is the part that prevents me from dealing with NHL games
-- scoreDrop finds the beginning of the scoreboard and discards everything before it
scoreDrop :: [Tag String] -> [Tag String]
scoreDrop [] = []
scoreDrop list@(x:xs)
    | x ~== TagOpen "div" [] && fromAttrib "class" x =~ "spanCol[0-9]?" = list
    | otherwise = scoreDrop xs

-- scoreTake finds the first distinctive tag after the end of the scoreboard and discards all information after it
scoreTake :: [Tag String] -> [Tag String]
scoreTake [] = []
scoreTake (x:xs)
    | x ~== TagOpen "div" [] && fromAttrib "class" x =~ "score[a-zA-Z]*Key" = []
    | otherwise = x : scoreTake xs


-- takes in the scoreboard, eliminates all non-finished games, and isolates the parts that needed for future use
gameResult :: [Tag String] -> [[[Tag String]]]
gameResult = map (splitTeam) . scoreFinal . splitGames where
    splitGames = partitions (~== TagOpen "table" [("class", "lineScore")])
    splitTeam = partitions (~== TagOpen "tr" [("id", "final"), ("align", "right")])


-- discards information from games that have not yet gone final
scoreFinal :: [[Tag String]] -> [[Tag String]]
scoreFinal [] = []
scoreFinal (xs:ys)
    -- two checks is redundant, but you never can be too sure
    | elem (TagOpen "td" [("class","finalscore"),("align","center")]) xs && elem (TagOpen "font" [("class","winarrow")]) xs = xs : scoreFinal ys
    | otherwise = scoreFinal ys


-- eliminates the junk information like score per period that come after the important information
trimTags :: [[[Tag String]]] -> [[[Tag String]]]
trimTags [] = []
trimTags (xs:ys) = map (takeWhile (/= TagClose "td")) xs : trimTags ys


-- gets rid of all tags that do not contain some information, leaving only tags with needed information
hasText :: [[[Tag String]]] -> [[[Tag String]]]
hasText [] = []
hasText (xs:ys) = map (filter isTagText) xs : hasText ys


-- retrieves the information from inside the tags
produceText :: [[[Tag String]]] -> [[[String]]]
produceText [] = []
produceText (xs:ys) = map (map fromTagText) xs : produceText ys


-- avoids issues with not counting games from a doubleheader by adding a '1' to the end of the winning team name
-- this '1' is eliminated later in the process and is only a temporary marker
doubleheader :: [[[String]]] -> [[[String]]]
doubleheader [] = []
doubleheader (xs:ys)
    | xs `elem` ys = map decrease xs : doubleheader ys
    | otherwise = xs : doubleheader ys where
        decrease [] = []
        decrease list@(xs:ys) = (xs ++ "1") : drop 1 list


-- makes a new evaluation date when using user-specified dates
newEvalDate :: String -> Day -> Day -> [Glicko] -> [Glicko]
newEvalDate league sd td gs
    | diffDays (addDays (n league) sd) td < 0 = newEvalDate league (addDays (n league) sd) td (map (runGlicko league) gs)
    | otherwise = gs


-- wraps up a season, if the user has specified that it is the end
finishUp :: String -> IO ()
finishUp s = do
    putStrLn "Ending the season."
    fp <- fmap (lines . show . BS.unpack) $ BS.readFile ("/Users/Swanson/Programs/Haskell/glicko-ratingstest/glicko" ++ s)
    let teams = map (runGlicko s) (read (fp !! 1) :: [Glicko])
    let output = fp !! 2 ++ "\n" ++ (show teams) ++ "\n" ++ fp !! 2
    writeFile ("/Users/Swanson/Programs/Haskell/glicko-ratingstest/glicko" ++ s) output
    putStrLn "Season successfully ended."


----
----
----
----
----
----

-- all commnand line options
-- Maybe type makes it easy to figure out what the user inputted
-- booleans are for flags that either happen or do not, also easy to figure out
data Options = Options {
    optBegin :: Maybe Day,
    optEnd :: Maybe Day,
    optFinish :: Maybe Day,
    optLeague :: Maybe String,
    optStart :: Maybe Day,
    optVersion :: Bool,
    optHelp :: Bool,
} deriving Show

-- the default values signal when the user has not given a value for the specific flag
defaultOptions :: Options
defaultOptions = Options {
    optBegin = Nothing,
    optEnd = Nothing,
    optFinish = Nothing,
    optLeague = Nothing,
    optStart = Nothing,
    optVersion = False,
    optHelp = False,
}

-- all the valid command line options available for use
options :: [OptDescr (Options -> Options)]
options = [
    Option ['b'] ["begin"]
        (ReqArg (\ arg opt -> opt {optBegin = Just $ readDate arg}) "BEGINDATE")
        "date on which start a season. date format ddmmyyyy. incompatable with -s",
    Option ['e'] ["end"] 
        (ReqArg (\ arg opt -> opt {optEnd = Just $ readDate arg}) "ENDDATE") 
        "date to stop retrieving information. date format ddmmyyyy. incompatable with -f",
    Option ['f'] ["finish"] 
        (ReqArg (\ arg opt -> opt {optFinish = Just $ readDate arg}) "FINISHDATE") 
        "date after which to end the season. date format ddmmyyyy. incompatable with -e",
    Option ['h'] ["help"] 
        (NoArg (\ opt -> opt {optHelp = True})) 
        "show help information. does not run other arguments",
    Option ['l'] ["league"] 
        (ReqArg (\ arg opt -> opt {optLeague = Just (map toLower arg)}) "LEAGUENAME") 
        "league for which to process rankings. currently only works for MLB and NBA",
    Option ['s'] ["start"] 
        (ReqArg (\ arg opt -> opt {optStart = Just $ readDate arg}) "STARTDATE") 
        "date to start retrieving information. date format ddmmyyyy. incompatable with -b",
    Option ['v'] ["version"] 
        (NoArg (\ opt -> opt {optVersion = True})) 
        "show version number. does not run other arguments"
    ]


-- parses a user inputted date
readDate :: String -> Day
readDate s@(d1:d2:m1:m2:y) = case foldr ((&&) . isDigit) True s of
    True -> error $ "could not read user date" ++ usageInfo header options
    _ -> case length s of
        8 -> fromGregorian (read y :: Integer) (read (m1:[m2]) :: Int) (read (d1:[d2]) :: Int)
        _ -> error $ "could not read user date" ++ usageInfo header options

-- shows the version number and exits
showVersion :: Bool -> IO ()
showVersion bool = when bool $ do
    prog <- getProgName
    putStrLn $ prog ++ ": version 1.1"
    exitWith ExitSuccess

-- shows all help information and exits
showHelp :: Bool -> IO ()
showHelp bool = when bool $ do
    putStrLn $ usageInfo header options
    exitWith ExitSuccess

-- parses the league name and determines if it is valid
valLeague :: Maybe String -> IO String
valLeague (Just league) = case n league of
    0 -> error $ league ++ " is not a recognized league name"
    _ -> return league
valLeague _ = error $ "Please provide a league name"

-- finds the designated start date, whether from user input or from file
findStart :: String -> Maybe Day -> Maybe Day -> IO Day
findStart _ (Just _) (Just _) = error $ "please select one of -b and -s"
findStart _ (Just begin) _ = return begin
findStart _ _ (Just start) = return start
findStart fp _ _ = do
    let date = read fp :: (Integer, Int, Int)
    return $ fromGregorian (tup1 date) (tup2 date) (tup3 date)

-- finds the end date from command line arguments if given, otherwise ends on the current day
findEnd :: Maybe Day -> Maybe Day -> IO Day
findEnd (Just _) (Just _) = error $ "please select one of -e and -f"
findEnd (Just end) _ = return end
findEnd _ (Just finish) = return finish
findEnd _ _ = fmap utctDay getCurrentTime

-- determines the date on which to evalute ratings
evalDate :: String -> Maybe Day -> Day -> IO Day
evalDate _ (Just _) oldDate = return $ addDays (-1) oldDate
evalDate fp _ _ = do
    let date = read fp :: (Integer, Int, Int)
    return $ fromGregorian (tup1 date) (tup2 date) (tup3 date)

-- if starting state is user designated, determines if team ratings should be evaluated or not
valTeams :: String -> Day -> Day -> [Glicko] -> [Glicko]
valTeams league eval oldDate teams = case diffDays (addDays (n league) eval) oldDate < 0 of
    True -> valTeams league (addDays (n league) eval) oldDate (map (runGlicko league) teams)
    _ -> teams

-- if valTeams determines that ratings must be evaluated, this functions evaluates their ratings
processTeam :: Bool -> String -> Day -> Day -> [Glicko] -> [Glicko]
processTeam True league oldEval oldDate teams = valTeams league oldEval oldDate teams
processTeam _ _ _ _ teams = teams



-- takes in all command line options and processes the information
parseArgs :: Options -> IO ()
parseArgs (Options {
        optBegin = begin,
        optEnd = end,
        optFinish = finish,
        optLeague = name,
        optStart = start,
        optVersion = version,
        optHelp = help}) = do
    
    showVersion version
    showHelp help
    league <- valLeague name
    fp <- fmap (lines . BS.unpack) $ BS.readFile ("/Users/Swanson/Programs/Haskell/glicko-ratingstest/glicko" ++ league)
    oldDate <- findStart (head fp) begin start
    curDate <- findEnd end finish
    eval <- evalDate (last fp) begin oldDate
    let teams = read (fp !! 1) :: [Glicko]
    let bool = foldr ((||) . isJust) False [begin, end, finish, start]
    oldEval <- evalDate (last fp) Nothing oldDate
    let team = processTeam bool league oldEval oldDate teams


    putStrLn $ "curDate = " ++ showGregorian curDate
    putStrLn $ "oldDate = " ++ showGregorian oldDate 
    putStrLn $ "league = " ++ league
    putStrLn $ "teams = "
    printList team 
    putStrLn $ "eval = " ++ showGregorian eval where
        printList = printer
        printer [] = putStrLn ""
        printer (x:xs) = do
            putStrLn $ "    " ++ show x
            printer xs


-- reads in the command line options and sends to parser
main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (flags@(x:xs), [], []) -> parseArgs $ foldl (flip id) defaultOptions flags

        -- if there are no arguments or any unrecognized or invalid options, gives usage information
        (_, _, msgs) -> error $ concat msgs ++ usageInfo header options


-- header for usage options
header :: String
header = "Usage: main [OPTION...]"
