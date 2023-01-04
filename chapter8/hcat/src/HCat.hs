{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HCat where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified Data.Time.Clock.POSIX as PosixClock
import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.Info
import System.IO 
import qualified System.IO.Error as IOError
import qualified System.Process as Process
import qualified Text.Printf as Printf


-- Command line argument processing
handleArgs :: IO (Either String FilePath)
handleArgs =
    parseArgs <$> Env.getArgs
    where
        parseArgs argumentList =
            case argumentList of
                [fname] -> Right fname
                []      -> Left "No filename provided"
                _       -> Left "Multiple files not supported"

-- Handle errors uniformly by turning an Either into an IO Error
eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) = Exception.throwIO . IOError.userError $ show e

--
-- Paging
--

-- Group lines of text into pages
groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n elems =
    let (hd, tl) = splitAt n elems
    in hd : groupsOf n tl

-- Word wrap 
wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
    | Text.length lineText <= lineLength = [lineText]
    | otherwise = 
        let
            (candidate, nextLines) = Text.splitAt lineLength lineText
            (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
        in firstLine : wordWrap lineLength (overflow <> nextLines)
    where
        softWrap hardwrappedText textIndex
            | textIndex <= 0 = (hardwrappedText, Text.empty)
            | Text.index hardwrappedText textIndex == ' ' =
                let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
                in (wrappedLine, Text.tail rest)
            | otherwise = softWrap hardwrappedText (textIndex - 1)

-- Terminal Dimensions
data ScreenDimensions = ScreenDimensions
    { screenRows :: Int
    , screenColumns :: Int
    } deriving Show


getTerminalSize :: IO ScreenDimensions
getTerminalSize =
    case System.Info.os of
        "darwin" -> tPutScreenDimensions
        "linux" -> tPutScreenDimensions
        _other -> pure $ ScreenDimensions 25 80
    where
        tPutScreenDimensions :: IO ScreenDimensions
        tPutScreenDimensions =
            Process.readProcess "tput" ["lines"] ""
            >>= \lines ->
                Process.readProcess "tput" ["cols"] ""
                >>= \ cols ->
                    let lines' = read $ init lines
                        cols' = read $ init cols
                    in pure $ ScreenDimensions lines' cols'


paginate :: ScreenDimensions -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows columns) text =
    let unwrappedLines = Text.lines text
        wrappedLines = concatMap (wordWrap columns) unwrappedLines
        pageLines = groupsOf rows unwrappedLines
    in map Text.unlines pageLines

-- Getting User Input
data ContinueCancel = Continue | Cancel 
    deriving (Eq, Show)

getContinue :: IO ContinueCancel
getContinue =
    hSetBuffering stdin NoBuffering
    >> hSetEcho stdin False
    >> hGetChar stdin
    >>= \case
        ' ' -> pure Continue
        'q' -> pure Cancel
        _ -> getContinue

-- Paging

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

showPages :: [Text.Text] -> IO ()
showPages [] = pure ()
showPages (page:pages) =
    clearScreen
    >> TextIO.putStrLn page
    >> getContinue
    >>= \case
        Continue -> showPages pages
        Cancel -> return ()

--
-- Status Line
--

data FileInfo = FileInfo
    { filePath :: FilePath
    , fileSize :: Int
    , fileMTime :: Clock.UTCTime
    , fileReadable :: Bool
    , fileWriteable :: Bool
    , fileExecutable :: Bool
} deriving Show


fileInfo :: FilePath -> IO FileInfo
fileInfo filePath =
    Directory.getPermissions filePath >>= \perms ->
        Directory.getModificationTime filePath >>= \mtime ->
            BS.readFile filePath >>= \contents ->
                let size = BS.length contents
                in pure FileInfo
                    { filePath = filePath
                    , fileSize = size
                    , fileMTime = mtime
                    , fileReadable = Directory.readable perms
                    , fileWriteable = Directory.writable perms
                    , fileExecutable = Directory.executable perms
                    }

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo{..} maxWidth totalPages currentPage =
    let
        statusLine = Text.pack $
            Printf.printf
                "%s / permissions: %s / %d bytes / modified: %s / page: %d of %d"
                    filePath permissionString fileSize timestamp currentPage totalPages
        permissionString = 
            [ if fileReadable then 'r' else '-'
            , if fileWriteable then 'w' else '-'
            , if fileExecutable then 'x' else '-'
            ]
        timestamp =
            TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
    in invertText (truncateStatus statusLine)
    where
        invertText inputStr =
            let
                reverseVideo = "\^[[7m"
                resetVideo = "\^[[0m"
            in reverseVideo <> inputStr <> resetVideo
        truncateStatus statusLine 
            | maxWidth <= 3 = ""
            | Text.length statusLine > maxWidth = Text.take (maxWidth - 3) statusLine <> "..."
            | otherwise = statusLine


runHCat :: IO ()
runHCat = 
    handleArgs
    >>= eitherToErr
    >>= flip openFile ReadMode
    >>= TextIO.hGetContents
    >>= \contents ->
        getTerminalSize >>= \termSize ->
            let pages = paginate termSize contents
            in showPages pages


