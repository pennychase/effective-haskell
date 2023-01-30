{-# LANGUAGE OverloadedStrings #-}

module FilePack where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import Data.Word (Word32)
import System.Posix.Types (FileMode, FileOffset)
import qualified System.Posix.Files as Posix
import Text.Read (readEither)

data FileData = FileData
    { fileName :: FilePath
    , fileSize :: Word32
    , filePermissions :: FileMode
    , fileData :: BS.ByteString
    } deriving (Eq, Read, Show)

newtype FilePack = 
    FilePack {getPackedFiles :: [FileData] } deriving (Eq, Read, Show)

packFiles :: FilePack -> BS.ByteString
packFiles filePack =
    B64.encode . BC.pack . show $ filePack

unpackFiles :: BS.ByteString -> Either String FilePack

unpackFiles serializedData =
    B64.decode serializedData >>= readEither . BC.unpack

-- Tests
samplePack = FilePack [FileData "foo.txt" 0 0 "hello world!/n this is just a test"]

testPackFile :: BS.ByteString
testPackFile = packFiles samplePack

testUnpackFile :: Either String FilePack
testUnpackFile = unpackFiles testPackFile

testRoundTrip :: FilePack -> Bool
testRoundTrip pack = (Right pack) == (unpackFiles $ packFiles pack)

-- Create test data from files

makeFileData :: FilePath -> IO FileData
makeFileData path = do 
    fileStatus <- Posix.getFileStatus path 
    contents <- BS.readFile path
    pure FileData    
        { fileName = path
        , fileSize = fromIntegral (Posix.fileSize fileStatus) :: Word32
        , filePermissions = Posix.fileMode fileStatus
        , fileData = contents
        }

makeFilePack :: [FilePath] -> IO FilePack
makeFilePack paths = FilePack <$> mapM makeFileData paths



