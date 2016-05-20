module Main where

import Control.Category ((>>>))
import Data.Either (partitionEithers)
import Data.List (intersperse)
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  fileContents <- readFile filePath
  let (errors, ints) = parseLines fileContents
  if length errors > 0
    then printErrors errors
    else printSum ints


printErrors :: [String] -> IO ()
printErrors = putStrLn . concat . intersperse "\n"


printSum :: [Integer] -> IO ()
printSum = putStrLn . show . sum


parseLines :: String -> ([String], [Integer])
parseLines =
  lines >>>
  enumerate >>>
  map parseLine >>>
  partitionEithers


enumerate :: [String] -> [(Integer, String)]
enumerate = zip [1..]


parseLine :: (Integer, String) -> Either String Integer
parseLine (lineNumber, line) =
  case readMaybe line of
    Just integer -> Right integer
    Nothing -> Left $ getErrorMessage lineNumber line


getErrorMessage :: Integer -> String -> String
getErrorMessage lineNumber line =
  printf "Wrong number '%s' at line: %d" line lineNumber
