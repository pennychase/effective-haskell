{-# LANGUAGE OverloadedStrings #-}
module LazyIOExamples where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.IORef

characterCounter :: FilePath -> IO (Text.Text -> Int)
characterCounter filePath = do
    haystack <- TextIO.readFile filePath
    pure $ \needle -> 
        Text.count needle haystack + Text.count needle (Text.pack filePath)

someExample :: FilePath -> IO (IORef Int)
someExample path = do
    countRef <- newIORef 0
    let somePath = complicatedPathFinding path
    counter <- characterCounter somePath
    writeIORef countRef (counter " ")
    pure countRef
    where
        complicatedPathFinding = id


someExampleStrict :: FilePath -> IO (IORef Int)
someExampleStrict path = do
    countRef <- newIORef 0
    let somePath = complicatedPathFinding path
    counter <- characterCounter somePath
    modifyIORef' countRef (const $ counter " ")
    pure countRef
    where
        complicatedPathFinding = id

