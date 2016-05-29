module Main where

import System.Environment
import Text.Read

toIndexed :: [a] -> [(a,Int)]
toIndexed xs = zip xs [1..]

parseData :: String -> (Either Int String)
parseData x =
  case readMaybe x of
   Just a -> Left a
   Nothing -> Right x

parseDataList :: [String] -> [Either Int String]
parseDataList xs = map parseData xs

isValidNum :: (Either Int String,Int) -> Bool
isValidNum (x,_) =
  case x of
   Left _ -> True
   otherwise -> False

isInvalidNum :: (Either Int String,Int) -> Bool
isInvalidNum n = not $ isValidNum n

dataToNumList :: [(Either Int String, Int)] -> [Int]
dataToNumList xs = map (\(Left x, i) -> x) (filter isValidNum xs)

dataToErrorList :: [(Either Int String, Int)] -> [String]
dataToErrorList xs = formatErrors $ map (\(Right x,i) -> (x,i))  $ filter isInvalidNum xs

formatErrors :: [(String, Int)] -> [String]
formatErrors = map (\(s,i) -> "Wrong number '" ++ s ++ "' at line: " ++ show i)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  let results = toIndexed . parseDataList . lines $ contents
  mapM_ putStrLn $ dataToErrorList results
  putStrLn . show . foldr (+) 0 . dataToNumList $ results

