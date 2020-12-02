import System.IO
import Data.List.Split

numberOfOccurences :: String -> Char -> Int
numberOfOccurences password letter = length $ filter (==letter) password

isValid :: String -> Char -> Int -> Int -> Bool
isValid password letter minOcc maxOcc
  | occurences < minOcc = False
  | occurences > maxOcc = False
  | otherwise = True
  where occurences = numberOfOccurences password letter

evaluatePassword :: [String] -> Bool
evaluatePassword line =
  isValid password letter min max
  where
    min = read (line !! 0) :: Int
    max = read (line !! 1) :: Int
    letter = (line !! 2) !! 0
    password = line !! 3

main :: IO ()
main = do
  infile <- readFile "day2_input.txt"
  let evaluatedPasswords = [ evaluatePassword (splitOneOf " -" x) | x <- lines infile]
  print $ length $ filter (==True) evaluatedPasswords
