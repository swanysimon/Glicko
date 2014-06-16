----
----
-- Glicko.hs
-- Haskell module implementing the Glicko rating system
-- this module contains primary command line handling 
-- functions
-- created by Simon Swanson
----
----

module Main where

import Scraper
import Rating

import Control.Monad
import Data.Time.Calendar
import Data.Time.Clock
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit


-- all available commnand line options, default values, and rules for use
data Options = Options {
    optBegin 	:: Maybe Day,
    optEnd 		:: Maybe Day,
    optFinish 	:: Maybe Day,
    optLeague 	:: Maybe String,
    optStart 	:: Maybe Day,
    optVersion 	:: Bool,
    optHelp 	:: Bool
}
	deriving Show


defaultOptions :: Options
defaultOptions = Options {
    optBegin 	= Nothing,
    optEnd 		= Nothing,
    optFinish 	= Nothing,
    optLeague 	= Nothing,
    optStart 	= Nothing,
    optVersion 	= False,
    optHelp 	= False,
}


options :: [OptDescr (Options -> Options)]
options = [
    Option ['b'] ["begin"]
        (ReqArg (\ arg opt -> opt {optBegin = Just $ readDate arg}) "BEGINDATE")
        "Date on which start a season. Date format ddmmyyyy. Incompatable with -s",
    Option ['e'] ["end"] 
        (ReqArg (\ arg opt -> opt {optEnd = Just $ readDate arg}) "ENDDATE") 
        "Date on which to stop retrieving information. Date format ddmmyyyy. Incompatable with -f",
    Option ['f'] ["finish"] 
        (ReqArg (\ arg opt -> opt {optFinish = Just $ readDate arg}) "FINISHDATE") 
        "Date after which to end the season. Date format ddmmyyyy. Incompatable with -e",
    Option ['h'] ["help"] 
        (NoArg (\ opt -> opt {optHelp = True})) 
        "Show help information. Does not run other arguments",
    Option ['l'] ["league"] 
        (ReqArg (\ arg opt -> opt {optLeague = Just (map toLower arg)}) "LEAGUENAME") 
        "League for which to process rankings. Currently only works for MLB and NBA",
    Option ['s'] ["start"] 
        (ReqArg (\ arg opt -> opt {optStart = Just $ readDate arg}) "STARTDATE") 
        "Date to start retrieving information. Date format ddmmyyyy. Incompatable with -b",
    Option ['v'] ["version"] 
        (NoArg (\ opt -> opt {optVersion = True})) 
        "Show version number. Does not run other arguments"
]


{-

-}
readDate :: String -> Day
readDate s@(d1:d2:m1:m2:y) = case foldr ((&&) . isDigit) True s || length s == 8 of
	True -> fromGregorian (read y :: Integer) (read (m1:[m2]) :: Int) (read (d1:[d2]) :: Int)
    _ -> error $ "could not read user date" ++ usageInfo header options


showVersion :: Bool -> IO ()
showVersion bool = when bool $ do
    prog <- getProgName
    putStrLn $ prog ++ ": version 1.2"
    exitWith ExitSuccess


showHelp :: Bool -> IO ()
showHelp bool = when bool $ do
    putStrLn $ usageInfo header options
    exitWith ExitSuccess


valLeague :: Maybe String -> IO String
valLeague (Just league) = case decay league of
    0 -> error $ league ++ " is not a recognized league name"
    _ -> return league
valLeague _ = error $ "Please provide a league name"


findStart :: String -> Maybe Day -> Maybe Day -> IO Day
findStart _ (Just _) (Just _) 	= error $ "please select one of -b and -s"
findStart _ (Just begin) _ 		= return begin
findStart _ _ (Just start) 		= return start
findStart dateFromFile _ _ 		= do
    let date = read dateFromFile :: (Integer, Int, Int)
    return $ fromGregorian (tup1 date) (tup2 date) (tup3 date)


findEnd :: Maybe Day -> Maybe Day -> IO Day
findEnd (Just _) (Just _) 	= error $ "please select one of -e and -f"
findEnd (Just end) _ 		= return end
findEnd _ (Just finish) 	= return finish
-- uses current date if none provided
findEnd _ _ 				= fmap utctDay getCurrentTime


evalDate :: String -> Maybe Day -> Day -> IO Day
evalDate _ (Just _) oldDate = return $ addDays (-1) oldDate
evalDate dateFromFile _ _ = do
    let date = read dateFromFile :: (Integer, Int, Int)
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
        optBegin 	= begin,
        optEnd 		= end,
        optFinish 	= finish,
        optLeague 	= name,
        optStart 	= start,
        optVersion 	= version,
        optHelp 	= help
		}) = do
    
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


main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (flags@(x:xs), [], []) -> parseArgs $ foldl (flip id) defaultOptions flags

        -- catches unrecognized or invalid options
        (_, _, msgs) -> error $ concat msgs ++ usageInfo header options


-- header for usage options
header :: String
header = "Usage: main [OPTION...]"

