{-# LANGUAGE TypeApplications #-}

module DirectoryTraversal where

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List
import System.Directory
import qualified Data.Set as Set
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map.Strict as Map
import Text.Printf


import Metrics


dropSuffix :: String -> String -> String
dropSuffix suffix s 
    | suffix `isSuffixOf` s = take (length s - length suffix) s
    | otherwise = s 


data FileType = FileTypeDirectory
              | FileTypeRegularFile
              | FileTypeOther

classifyFile :: FilePath -> IO FileType
classifyFile fname = do
    isDirectory <- doesDirectoryExist fname 
    isFile <- doesFileExist fname
    pure $ case (isDirectory, isFile) of
        (True, False) -> FileTypeDirectory
        (False, True) -> FileTypeRegularFile
        _otherwise    -> FileTypeOther

-- How to call
-- filenames: traverseDirectory <metrics> <dir> $ \file -> print file
-- size of filenames: traverseDirectory <metrics> <dir> $ \file -> print $ length file
-- file sizes: traverseDirectory <metrics> <dir> $ \file -> countBytes file >>= print

traverseDirectory :: Metrics -> FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory metrics rootPath action = do
    seenRef <- newIORef Set.empty

    let
        haveSeenDirectory canonicalPath =
            Set.member canonicalPath <$> readIORef seenRef

        addDirectoryToSeen canonicalPath = 
            modifyIORef seenRef $ Set.insert canonicalPath

        traverseSubdirectory subdirPath = do
            timeFunction metrics "traverseSubdirectory" $ do
                contents <- listDirectory subdirPath
                for_ contents $ \file' ->
                    handle @IOException (\ex -> print ex >> tickFailure metrics) $ do
                        let file = subdirPath <> "/" <> file'
                        canonicalPath <- canonicalizePath file
                        classification <- classifyFile canonicalPath
                        case classification of
                            FileTypeOther -> pure ()
                            FileTypeRegularFile -> action file
                            FileTypeDirectory -> do
                                alreadyProcessed <- haveSeenDirectory canonicalPath
                                when (not alreadyProcessed) $ do
                                    addDirectoryToSeen canonicalPath
                                    traverseSubdirectory canonicalPath
                tickSuccess metrics
                --pure result

    traverseSubdirectory (dropSuffix "/" rootPath)

-- To test traverseDirectory

-- Actions to pass - see comment at traverseDirectory on how to call
countBytes :: FilePath -> IO (FilePath, Integer)
countBytes path = do
    bytes <- fromIntegral . ByteString.length <$> ByteString.readFile path
    pure (path, bytes)

-- Using IORef to maintain state to enable a single directory traversal and store one file in memory
-- Need to create an empty Metrics to pass in with: metrics <- newMetrics
longestContents :: Metrics -> FilePath -> IO ByteString.ByteString 
longestContents metrics rootPath = do
    contentsRef <- newIORef ByteString.empty
    let
        takeLongrestFile a b =
            if ByteString.length a >= ByteString.length b
                then a
                else b

    traverseDirectory metrics rootPath $ \file -> do
        contents <- ByteString.readFile file
        modifyIORef contentsRef (takeLongrestFile contents)

    readIORef contentsRef

-- Directory processing with Metrics
-- Traverse directory. For each regular file, print the word count and update character frequency historgram.
-- At the end, print out histogram and metrics.

directorySummaryWithMetrics :: FilePath -> IO ()
directorySummaryWithMetrics root = do
    metrics <- newMetrics
    histogramRef <- newIORef (Map.empty :: Map.Map Char Int)
    traverseDirectory metrics root $ \file -> do
        putStrLn $ file <> ": "
        contents <- timeFunction metrics "TextIO.readFile" $ TextIO.readFile file

        timeFunction metrics "wordcount" $
            let wordCount = length $ Text.words contents
            in putStrLn $ "    word count: " <> show wordCount

        timeFunction metrics "histogram" $ do
            oldHistogram <- readIORef histogramRef
            let 
                addCharToHistogram histogram char =
                    Map.insertWith (+) char 1 histogram
                newHistogram = Text.foldl' addCharToHistogram oldHistogram contents
            writeIORef histogramRef newHistogram

    timeFunction metrics "print histogram" $ do
        histogram <- readIORef histogramRef
        putStrLn "Histogram Data:"
        for_ (Map.toList histogram) $ \(char, count) ->
            putStrLn $ printf "    %c: %d" char count

    displayMetrics metrics









