import System.IO
import Data.List.Split

-- PART 1
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

-- PART 2
inCorrectPosition :: String -> Char -> Int -> Int
inCorrectPosition password letter position =
  if password !! (position - 1) == letter
    then 1
    else 0

numberOfCorrectPositions :: String -> Char -> Int -> Int -> Int
numberOfCorrectPositions password letter pos1 pos2 = (inCorrectPosition password letter pos1) + (inCorrectPosition password letter pos2)

evaluatePassword2 :: [String] -> Bool
evaluatePassword2 line =
  (numberOfCorrectPositions password letter pos1 pos2) == 1
  where
    pos1 = read (line !! 0) :: Int
    pos2 = read (line !! 1) :: Int
    letter = (line !! 2) !! 0
    password = line !! 3

main :: IO ()
main = do
  infile <- readFile "day2_input.txt"
  let list = [ splitOneOf " -" x | x <- lines infile]

  let evaluatedPasswords = [ evaluatePassword x | x <- list]
  print $ length $ filter (==True) evaluatedPasswords

  let evaluatedPasswords2 = [ evaluatePassword2 x | x <- list]
  print $ length $ filter (==True) evaluatedPasswords2
