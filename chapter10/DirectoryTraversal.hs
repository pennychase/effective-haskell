{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List
import System.Directory
import qualified Data.Set as Set
import qualified Data.ByteString as ByteString


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
-- filenames: traverseDirectory "/tmp/test1" $ \file -> print file
-- size of filenames: traverseDirectory "/tmp/test1" $ \file -> print $ length file
-- file sizes: traverseDirectory "/tmp/test1" $ \file -> countBytes file >>= print

traverseDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory rootPath action = do
    seenRef <- newIORef Set.empty

    let
        haveSeenDirectory canonicalPath =
            Set.member canonicalPath <$> readIORef seenRef

        addDirectoryToSeen canonicalPath = 
            modifyIORef seenRef $ Set.insert canonicalPath

        traverseSubdirectory subdirPath = do
            contents <- listDirectory subdirPath
            for_ contents $ \file' ->
                handle @IOException (\_ -> pure ()) $ do
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

    traverseSubdirectory (dropSuffix "/" rootPath)

-- Recreate the original version by collecting results of the IO action in an IORef
traverseDirectory' :: FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory' rootPath action = do
    resultsRef <- newIORef []
    traverseDirectory rootPath $ \file -> do
        modifyIORef resultsRef (action file :)
    readIORef resultsRef

-- Actions to pass

countBytes :: FilePath -> IO (FilePath, Integer)
countBytes path = do
    bytes <- fromIntegral . ByteString.length <$> ByteString.readFile path
    pure (path, bytes)

-- Using IORef to maintain state to enable a single directory traversal and store one file in memory
longestContents :: FilePath -> IO ByteString.ByteString 
longestContents rootPath = do
    contentsRef <- newIORef ByteString.empty
    let
        takeLongrestFile a b =
            if ByteString.length a >= ByteString.length b
                then a
                else b

    traverseDirectory rootPath $ \file -> do
        contents <- ByteString.readFile file
        modifyIORef contentsRef (takeLongrestFile contents)

    readIORef contentsRef


main :: IO ()
main = pure ()


