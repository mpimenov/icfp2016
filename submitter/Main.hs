{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.Maybe
import Data.List
import qualified Data.ByteString.Lazy as L
import System.Process
import Text.Printf
import Control.Concurrent
import Data.Ratio
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

getSnapshots :: IO ()
getSnapshots = callProcess "curl" [ "--compressed"
                                  , "-L"
                                  , "-H Expect:"
                                  , "-H"
                                  , "X-API-Key: 246-2a711bdbced516bc9ce8f3205b413a58"
                                  , "http://2016sv.icfpcontest.org/api/snapshot/list"
                                  ]
                                  
lookupBlob :: String -> IO String
lookupBlob hash = readProcess "curl" [ "--compressed"
                                     , "-L"
                                     , "-H Expect:"
                                     , "-H"
                                     , "X-API-Key: 246-2a711bdbced516bc9ce8f3205b413a58"
                                     , link
                                     ] ""
    where link = printf "http://2016sv.icfpcontest.org/api/blob/%s" hash
    

solveAndWriteToFile :: String -> IO ()
solveAndWriteToFile input = do
  let problem =  (evalState nextProblem) input
  let solution = solve problem
  writeFile "/tmp/solution.txt" (concat $ intersperse "\n" (toStrings solution))

runAndSubmitSolution :: (Int,String) -> IO ()
runAndSubmitSolution (id, hash) = do
    inp <- lookupBlob hash
    solveAndWriteToFile inp
    let problemIdStr = printf "problem_id=%d" id
    callProcess "curl" [ "--compressed"
                       , "-L"
                       , "-H"
                       , "Expect:"
                       , "-H"
                       , "X-API-Key: 246-2a711bdbced516bc9ce8f3205b413a58"
                       , "-F"
                       , problemIdStr
                       , "-F"
                       , "solution_spec=@/tmp/solution.txt"
                       , "http://2016sv.icfpcontest.org/api/solution/submit"
                       ]
    putStrLn ""
    threadDelay 1000000


snapshotHash = "f8862c595bc6a7d7be83f5af363d5b88e6f27fad"

main = do
{-
    a <- readJSON "/tmp/1.json" :: IO Snapshots
    putStrLn $ show a
    b <- readJSON "/tmp/2.json" :: IO Snapshot
    putStrLn $ show (problemIdsAndHashes . problems $ b)
    --getSnapshots
    --lookupBlob "44f66105e0136a9ea0a4fa4b055c35318ed8840f"
    runAndSubmitSolution 1
-}
    --lookupBlob snapshotHash
    snapshot <- readJSON "/tmp/snapshot.json" :: IO Snapshot
    let ps = problemIdsAndHashes . problems $ snapshot
    
    -- writeFile "/tmp/solution.txt" 
    forM_ [100..1099] $ \x -> runAndSubmitSolution (ps!!x)
