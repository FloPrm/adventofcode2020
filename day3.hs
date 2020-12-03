import System.IO

getCol :: Int -> Int -> Int
getCol lineLength col =
  if col + 3 >= lineLength
    then col + 3 - lineLength
    else col + 3

addTree :: [String] -> Int -> Int -> Int
addTree list line col =
  if ((list !! line) !! col) == '#'
    then 1
    else 0

countTrees :: [String] -> Int -> Int -> Int -> Int
countTrees list count line col =
  if line >= (length list)
    then count
    else countTrees list new_count new_line new_col
    where
      new_count = count + addTree list line col
      new_line = line + 1
      new_col = getCol (length(list !! line)) col

main :: IO ()
main = do
  infile <- readFile "day3_input.txt"
  let list = [ x | x <- lines infile]
  print(countTrees list 0 0 0)