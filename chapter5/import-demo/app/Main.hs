module Main where

import Data.Char (isPrint)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

countNonPrintableChars :: String -> Int
countNonPrintableChars = 
  Prelude.length . Prelude.filter (not . isPrint)

countNonPrintableCharsInText :: Text -> Int
countNonPrintableCharsInText =
  T.length . T.filter (not . isPrint) . decodeUtf8 . encodeUtf8 

countNonPrintableCharsStringAndText :: String -> (Int, Int)
countNonPrintableCharsStringAndText input =
  (countNonPrintableChars input, countNonPrintableCharsInText $ T.pack input)



main :: IO ()
main =
  print $ countNonPrintableCharsStringAndText "\v\t\ahello\r\n"