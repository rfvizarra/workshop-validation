module Main where

import Control.Category ((>>>))
import Data.Either (either)
import Data.List (intersperse)
import Data.Monoid ((<>))
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  fileContents <- readFile filePath
  validate printErrors printSum (parseLines fileContents)


printErrors :: [String] -> IO ()
printErrors = putStrLn . concat . intersperse "\n"


printSum :: [Integer] -> IO ()
printSum = putStrLn . show . sum


parseLines :: String -> Validation [String] [Integer]
parseLines =
  lines >>>
  enumerate >>>
  map parseLine >>>
  sequenceA


enumerate :: [String] -> [(Integer, String)]
enumerate = zip [1..]


parseLine :: (Integer, String) -> Validation [String] Integer
parseLine (lineNumber, line) =
  case readMaybe line of
    Just integer -> Valid integer
    Nothing -> Invalid [getErrorMessage lineNumber line]


getErrorMessage :: Integer -> String -> String
getErrorMessage lineNumber line =
  printf "Wrong number '%s' at line: %d" line lineNumber


data Validation e a =
  Invalid e |
  Valid a
  deriving (Eq, Show, Ord)


instance Functor (Validation e) where
  -- fmap :: (a -> b) -> (Validation e) a -> (Validation e) b
  fmap _ (Invalid e) = Invalid e
  fmap f (Valid a) = Valid (f a)


instance Monoid e => Applicative (Validation e) where
  -- pure :: a -> (Validation e) a
  pure = Valid

  -- (<*>) :: (Validation e) (a -> b) -> (Validation e) a -> (Validation e) b
  (Invalid a) <*> (Invalid b) = Invalid (a <> b)
  (Invalid a) <*> (Valid _)   = Invalid a
  (Valid _)   <*> (Invalid b) = Invalid b
  (Valid f)   <*> (Valid a)   = Valid (f a)


validate :: (e -> b) -> (a -> b) -> Validation e a -> b
validate handleInvalid handleValid validation =
  case validation of
    Valid a -> handleValid a
    Invalid e -> handleInvalid e
