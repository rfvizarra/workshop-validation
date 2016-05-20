module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  fileContents <- readFile filePath
  let result = sumLines fileContents
  putStrLn $ show result


sumLines :: String -> Integer
sumLines = sum . map read . lines
