import System.IO

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

iterateFirst :: [Integer] -> Int -> Integer
iterateFirst iList index =
    if index == length iList
    then 0
    else nextFirstValue iList index

nextFirstValue :: [Integer] -> Int -> Integer
nextFirstValue iList index
  | result <= 0 = iterateFirst iList (index + 1)
  | otherwise  = result
  where result = iterateSecond iList (iList !! index) index

iterateSecond :: [Integer] -> Integer -> Int -> Integer
iterateSecond iList firstElem index =
  if index == length iList
    then 0
  else nextSecondValue iList firstElem index

nextSecondValue :: [Integer] -> Integer -> Int -> Integer
nextSecondValue iList firstElem index
  | result <= 0 = iterateSecond iList firstElem (index + 1)
  | otherwise  = result
  where result = iterateThird iList firstElem (iList !! index) index

iterateThird :: [Integer] -> Integer -> Integer -> Int -> Integer
iterateThird iList firstElem secondElem index =
  if index == length iList
    then 0
  else if firstElem + secondElem + (iList !! index) == 2020
      then firstElem * secondElem * iList !! index
      else iterateThird iList firstElem secondElem (index + 1)

main :: IO ()
main = do
  handle <- openFile "day1_input.txt" ReadMode
  contents <- hGetContents handle
  let list = map read $ words [if c == ';' then ' ' else c|c <- contents] :: [Integer]

  print(iterateFirst list 0)

  hClose handle