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
            timeFunction metrics "traversweSubdirectory" $ do
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


-- Actions to pass
countBytes :: FilePath -> IO (FilePath, Integer)
countBytes path = do
    bytes <- fromIntegral . ByteString.length <$> ByteString.readFile path
    pure (path, bytes)

-- Using IORef to maintain state to enable a single directory traversal and store one file in memory
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




