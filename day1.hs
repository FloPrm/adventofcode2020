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
  where result = iterateSecond iList (iList !! index) 0

iterateSecond :: [Integer] -> Integer -> Int -> Integer
iterateSecond iList firstElem index =
  if index == length iList
    then 0
  else if is2020 firstElem (iList !! index)
      then firstElem * iList !! index
      else iterateSecond iList firstElem (index + 1)


is2020 :: Integer -> Integer -> Bool
is2020 x y = (x + y ) == 2020

main :: IO ()
main = do
  handle <- openFile "day1_input.txt" ReadMode
  contents <- hGetContents handle
  let list = map read $ words [if c == ';' then ' ' else c|c <- contents] :: [Integer]

  print(iterateFirst list 0)

  hClose handle