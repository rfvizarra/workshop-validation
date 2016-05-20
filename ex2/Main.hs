module Main where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  fileContents <- readFile filePath
  let result = sumLines fileContents
  putStrLn $ show result


sumLines :: String -> Integer
sumLines = sum . map numOrZero . lines
  where
    numOrZero :: String -> Integer
    numOrZero = fromMaybe 0 . readMaybe
