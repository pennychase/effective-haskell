module InterleaveFiles where

import qualified Data.List

interleaveLines :: String -> String -> String
interleaveLines a b =
    unlines . concat . Data.List.transpose $ [lines a, lines b]

-- Using bind
interleaveFiles :: FilePath -> FilePath -> FilePath -> IO String
interleaveFiles file1 file2 outfile =
    readFile file1 >>= \contents1 ->
        readFile file2 >>= \contents2 ->
            putStrLn "I've read two files" >>
            let contents3 = interleaveLines contents1 contents2
            in writeFile outfile contents3 >> return contents3

-- Using do notation
interleaveFiles' :: FilePath -> FilePath -> FilePath -> IO String
interleaveFiles' file1 file2 outfile = do
    contents1 <- readFile file1
    contents2 <- readFile file2
    putStrLn "I've read two files"
    let contents3 = interleaveLines contents1 contents2
    writeFile outfile contents3
    return contents3