{-# LANGUAGE RecordWildCards #-}

module Metrics where

import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Time.Clock ( diffUTCTime
                       , getCurrentTime
                       , nominalDiffTimeToSeconds
                       )
import Data.Maybe ( fromMaybe )
import Text.Printf
import System.CPUTime

data AppMetrics = AppMetrics
    { successCount :: Int
    , failureCount :: Int
    , callDuration :: Map.Map String Double
    } deriving (Eq, Show)

newtype Metrics = Metrics { appMetricsStore :: IORef AppMetrics }

newMetrics :: IO Metrics
newMetrics =
    let
        emptyAppMetrics = AppMetrics
            { successCount = 0
            , failureCount = 0
            , callDuration = Map.empty
            }            
    in Metrics <$> newIORef emptyAppMetrics

tickSuccess :: Metrics ->  IO ()
tickSuccess (Metrics metrics) = modifyIORef metrics $ \m ->
    m { successCount = 1 + successCount m }

tickFailure :: Metrics ->  IO ()
tickFailure (Metrics metrics) = modifyIORef metrics $ \m ->
    m { failureCount = 1 + failureCount m }

-- This is the timing code from the book, but everything was returning 0 (clock resolution?)
{-
timeFunction' :: Metrics -> String -> IO a -> IO a
timeFunction' (Metrics metrics) actionName action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime

    modifyIORef metrics $ \oldMetrics ->
        let
            oldDurationValue = fromMaybe 0 $ Map.lookup actionName (callDuration oldMetrics)
            runDuration = floor . nominalDiffTimeToSeconds $ diffUTCTime endTime startTime
            newDurationValue = oldDurationValue + runDuration
        in oldMetrics { callDuration = Map.insert actionName newDurationValue $ callDuration oldMetrics}

    pure result
-}

-- This code uses getCPUTime and returns the result as a double so we have fractional seconds.
-- Uses the code in the timeIt library to do the timing (similar to the blook but using getCPUTime)
-- and return the result in seconds
timeFunction :: Metrics -> String -> IO a -> IO a
timeFunction (Metrics metrics) actionName action = do
    startTime <- getCPUTime
    result <- action
    endTime <- getCPUTime

    modifyIORef metrics $ \oldMetrics ->
        let
            oldDurationValue = fromMaybe 0 $ Map.lookup actionName (callDuration oldMetrics)
            runDuration = fromIntegral (endTime - startTime) * 1e-12
            newDurationValue = oldDurationValue + runDuration
        in oldMetrics { callDuration = Map.insert actionName newDurationValue $ callDuration oldMetrics}

    pure result

displayMetrics :: Metrics -> IO ()
displayMetrics (Metrics metrics) = 
    readIORef metrics >>= display
    where
        display AppMetrics{..} = do
            putStrLn $ "sucesses: " <> show successCount
            putStrLn $ "failures: " <> show failureCount
            for_ (Map.assocs callDuration) $ \(k,v) ->
                putStrLn $ printf "Time spent in \"%s\": %6.2fs" k v

-- Initial implementation
metrics :: IO (IORef AppMetrics)
metrics = newIORef AppMetrics 
    { successCount = 0
    , failureCount = 0
    , callDuration = Map.empty
    }

printMetrics :: IO ()
printMetrics = metrics >>= readIORef >>= print

incrementSuccess :: IO ()
incrementSuccess = 
    metrics >>= flip modifyIORef incrementSuccess
    where
        incrementSuccess m = m { successCount = 1 + successCount m}

successfullyPrintHello :: IO ()
successfullyPrintHello = do
    print "Hello"
    incrementSuccess

printHelloAndMetrics :: IO ()
printHelloAndMetrics = do
    successfullyPrintHello
    printMetrics