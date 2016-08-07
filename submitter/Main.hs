{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.List
import Data.Maybe
import Data.Ratio
import System.Exit
import System.IO
import System.IO.Error
import System.Process
import Text.Printf
import qualified Data.ByteString.Lazy as L


import Folding
import Geom
import Problem
import Solution

                   
data RankingItem = RankingItem { resemblance :: Double
                               , solution_size :: Int
                               }
                               deriving (Show, Eq) 
                   
data ProblemDesc = ProblemDesc { publish_time :: Int
                               , solution_size1 :: Int
                               , problem_id :: Int
                               , owner :: String
                               , problem_size :: Int
                               , problem_spec_hash :: String
                               , ranking :: [RankingItem]
                               } 
                               deriving (Show, Eq)

data Player = Player { username :: String
                     , score :: Double
                     }
                     deriving (Show, Eq)
              
data PlayerNames = PlayerNames { username1 :: String
                               , display_name :: String
                               }
                               deriving (Show, Eq)       
      

data Snapshot = Snapshot { problems :: [ProblemDesc]
                         , snapshot_time :: Int
                         , leaderboard :: [Player]
                         , users :: [PlayerNames]
                         }
                         deriving (Show, Eq)

data SnapshotDescription = SnapshotDescription { snapshot_time1 :: Int
                                               , snapshot_hash :: String
                                               }
                                               deriving (Show, Eq)                              
                                                  
data Snapshots = Snapshots { status :: Bool
                           , snapshots :: [SnapshotDescription]
                           }
                           deriving (Show, Eq)

instance FromJSON RankingItem where
    parseJSON (Object v) = RankingItem <$>
                           v .: "resemblance" <*>
                           v .: "solution_size"
    parseJSON _ = mzero

instance FromJSON ProblemDesc where
    parseJSON (Object v) = ProblemDesc <$>
                           v .: "publish_time" <*>
                           v .: "solution_size" <*>
                           v .: "problem_id" <*>
                           v .: "owner" <*>
                           v .: "problem_size" <*>
                           v .: "problem_spec_hash" <*>
                           v .: "ranking"
    parseJSON _ = mzero

instance FromJSON Player where
    parseJSON (Object v) = Player <$>
                           v .: "username" <*>
                           v .: "score"
    parseJSON _ = mzero

instance FromJSON PlayerNames where
    parseJSON (Object v) = PlayerNames <$>
                           v .: "username" <*>
                           v .: "display_name"
    parseJSON _ = mzero

instance FromJSON Snapshot where
    parseJSON (Object v) = Snapshot <$>
                           v .: "problems" <*>
                           v .: "snapshot_time" <*>
                           v .: "leaderboard" <*>
                           v .: "users"
    parseJSON _ = mzero
    
instance FromJSON SnapshotDescription where
    parseJSON (Object v) = SnapshotDescription <$>
                           v .: "snapshot_time" <*>
                           v .: "snapshot_hash"
    parseJSON _ = mzero

instance FromJSON Snapshots where
    parseJSON (Object v) = Snapshots <$>
                           v .: "ok" <*>
                           v .: "snapshots"
    parseJSON _ = mzero


readJSON :: FromJSON a => FilePath -> IO a
readJSON = liftM (fromJust . decode) . L.readFile


problemIdsAndHashes :: [ProblemDesc] -> [(Int, String)]
problemIdsAndHashes [] = []
problemIdsAndHashes (p:ps) = (problem_id p, problem_spec_hash p):(problemIdsAndHashes ps)

runCurl :: [String] -> IO String
runCurl args = do
    (status, stdout, stderr) <- readProcessWithExitCode "curl" args ""
    case status of
        ExitSuccess      -> return stdout
        ExitFailure code -> fail $ "Can't run curl with args" ++ (show args) ++ "\nstderr:" ++ stderr ++ "\nexit code:" ++ (show code)

getSnapshots :: IO String
getSnapshots = do
    runCurl [ "--compressed"
             , "-L"
             , "-H Expect:"
             , "-H"
             , printf "X-API-Key: %s" apiKey
             , "http://2016sv.icfpcontest.org/api/snapshot/list"
             ]


tryRead :: String -> IO String
tryRead filename = do
    contents <- readFile filename
    return contents

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of Just path -> putStrLn $ "File does not exist at: " ++ path
                                 Nothing -> putStrLn "bad location"
    | otherwise = ioError e


lookupBlob :: Int -> String -> IO String
lookupBlob id hash = do
    --let filename = inputPath ++ (show id) ++ ".in"
    --contents <- (tryRead `catch` handler)
    runCurl [ "--compressed"
            , "-L"
            , "-H Expect:"
            , "-H"
            , printf "X-API-Key: %s" apiKey
            , printf "http://2016sv.icfpcontest.org/api/blob/%s" hash
            ]


solveAndWriteToFile :: String -> IO ()
solveAndWriteToFile input = do
  let problem  = (evalState nextProblem) input
  let solution = solve problem
  writeFile tmpSolutionPath (concat $ intersperse "\n" (toStrings solution))

runAndSubmitSolution :: (Int,String) -> IO String
runAndSubmitSolution (id, hash) = do
    inp <- lookupBlob id hash
    solveAndWriteToFile inp
    result <- runCurl [ "--compressed"
                      , "-L"
                      , "-H"
                      , "Expect:"
                      , "-H"
                      , printf "X-API-Key: %s" apiKey
                      , "-F"
                      , printf "problem_id=%d" id
                      , "-F"
                      , printf "solution_spec=@%s" tmpSolutionPath
                      , "http://2016sv.icfpcontest.org/api/solution/submit"
                      ]
    threadDelay 1000000
    return result


apiKey = "246-2a711bdbced516bc9ce8f3205b413a58" :: String
tmpSolutionPath = "/tmp/solution.txt" :: String
--snapshotHash = "f8862c595bc6a7d7be83f5af363d5b88e6f27fad" :: String
snapshotHash = "96a0615f81c77be7dbb5a7a40c8c0912c67d6841" :: String
inputPath = "../input/" :: String


main = do
    --snapshots <- getSnapshots
    --putStrLn $ show snapshots

    --snap <- lookupBlob 0 snapshotHash
    --putStrLn $ show snap

    snapshot <- readJSON "/tmp/snapshot.json" :: IO Snapshot
    --putStrLn (show snapshot)

    let ps = problemIdsAndHashes . problems $ snapshot
    --putStrLn $ (show (length ps))
    forM_ [1600..1800] $ \x -> do
      s <- runAndSubmitSolution (ps!!x)
      putStrLn s
