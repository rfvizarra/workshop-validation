module Main where


import System.Environment (getArgs)
import Text.Read
main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  fileContents <- readFile filePath
  putStrLn . show . sumFileContents $ fileContents

hasData :: (Num a) => Maybe a -> Bool
hasData n =
  case n of
   Just _ -> True
   _ -> False

getNum :: (Num a) => Maybe a -> a
getNum (Just x) = x

sumFileContents :: (Num a, Read a) => String -> a
sumFileContents = sum . map getNum . filter hasData . map readMaybe . lines
