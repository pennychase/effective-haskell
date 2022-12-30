module Main where

import qualified System.Environment as Sys
import Calculator (Operator(..), calculate, toOperator)

createArgs :: [String] -> (Int, Int)
createArgs strs =
  (read $ strs !! 0, read $ strs !! 1)

-- Part 1 - the two numbers are passed in as command line arguments
-- main :: IO ()
-- main =
--   Sys.getArgs >>= (\args -> putStrLn . show $ calculate Add (createArgs args))

-- Part 2 - Pass in operator and numbers on command line
main :: IO ()
main =
  Sys.getArgs >>= \args ->
  let op = toOperator (head args)
      operands = createArgs (tail args)
  in
    case op of
      Nothing -> putStrLn "Unknown operator"
      (Just op') -> putStrLn . show $ calculate op' operands
