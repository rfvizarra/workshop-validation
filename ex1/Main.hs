module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  fileContents <- readFile filePath
  putStrLn . show . sumFileContents $ fileContents


sumFileContents :: (Num a, Read a) => String -> a
sumFileContents = sum . map  read . lines
