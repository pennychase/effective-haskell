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


traverseDirectory :: FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory rootPath action = do
    seenRef <- newIORef Set.empty
    resultRef <- newIORef []

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
                        FileTypeRegularFile -> modifyIORef resultRef (\results -> action file : results)
                        FileTypeDirectory -> do
                            alreadyProcessed <- haveSeenDirectory canonicalPath
                            when (not alreadyProcessed) $ do
                                addDirectoryToSeen canonicalPath
                                traverseSubdirectory canonicalPath

    traverseSubdirectory (dropSuffix "/" rootPath)
    readIORef resultRef

-- Actions to pass

-- If called as 'traverseDirectory <dir> countBytes' nothing is returned since returns IO [IO (FilePath, Integer)]
-- can call as 'join . fmap sequence $ traverseDirectory <dir> countBytes'
countBytes :: FilePath -> IO (FilePath, Integer)
countBytes path = do
    bytes <- fromIntegral . ByteString.length <$> ByteString.readFile path
    pure (path, bytes)

main :: IO ()
main = pure ()


