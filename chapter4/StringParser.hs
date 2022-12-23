module StringParser where

-- StringParser is a newtype that wraps the function that runs StringParsers
-- The book uses data but newtype is more idiomatic, since we're wrapping a single type
newtype StringParser = 
    StringParser { runStringParser :: String -> (String, String) }

-- Define a couple of parsers
takeCharacters :: Int -> StringParser
takeCharacters numChars = StringParser $ \someString -> 
    splitAt numChars someString

getNextWord :: StringParser
getNextWord = StringParser $ \someString ->
    case break (== ' ') someString of
        (nextWord, "") -> (nextWord, "")
        (nextWord, rest) -> (nextWord, tail rest)  -- remove the leading space from rest

-- Combine parsers
combineParsers :: StringParser -> StringParser -> StringParser
combineParsers firstParser secondParser = StringParser $ \someString ->
    let (_firstPart, firstResult) = runStringParser firstParser someString
    in runStringParser secondParser firstResult

-- Some example combinations
getNextWordAfterTenLetters :: StringParser
getNextWordAfterTenLetters = combineParsers (takeCharacters 10) getNextWord

tenLettersAfterGetNextWord :: StringParser
tenLettersAfterGetNextWord = combineParsers getNextWord (takeCharacters 10)

-- run a parser and return the result (in the first part of the tuple, ignoring the rest of the data in the second part)
parseString :: StringParser -> String -> String
parseString parser inputString = fst $ runStringParser parser inputString




