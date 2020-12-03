import System.IO

colSlopes :: [Int]
colSlopes = [1, 3, 5, 7, 1]

lineSlopes :: [Int]
lineSlopes = [1, 1, 1, 1, 2]

data Slope = Slope {
  count :: Int,
  lineShift :: Int,
  colShift :: Int,
  lineIndex :: Int,
  colIndex :: Int
} deriving (Show)

addTree :: [String] -> Int -> Int -> Int
addTree list line col =
  if ((list !! line) !! col) == '#'
    then 1
    else 0

getCol :: Int -> Slope -> Int
getCol lineLength slope =
  if col + slopeShift >= lineLength
    then col + slopeShift - lineLength
    else col + slopeShift
  where
    slopeShift = colShift slope
    col = colIndex slope

getSlopeLine :: Slope -> Int
getSlopeLine slope =
  (lineIndex slope) + (lineShift slope)


updateSlope :: [String] -> Slope -> Int -> Slope
updateSlope list slope currentIndex =
  if currentIndex == lineIndex slope
    then Slope { count = count slope + (addTree list currentIndex (colIndex slope)), lineShift = lineShift slope, colShift = colShift slope, lineIndex = getSlopeLine slope, colIndex = getCol lineLength slope  }
    else slope
    where
      lineLength =  length $ list !! currentIndex

countTrees :: [String] -> [Slope] -> Int -> [Int]
countTrees list slopes index =
  if index == (length list)
    then [ count slope | slope <- slopes]
    else countTrees list [ updateSlope list slope index | slope <- slopes] (index + 1)


main :: IO ()
main = do
  infile <- readFile "day3_input.txt"
  let list = [ x | x <- lines infile]

  let slope1 = Slope { count = 0, lineShift = 1, colShift = 1, lineIndex = 0, colIndex = 0 }
  let slope2 = Slope { count = 0, lineShift = 1, colShift = 3, lineIndex = 0, colIndex = 0 }
  let slope3 = Slope { count = 0, lineShift = 1, colShift = 5, lineIndex = 0, colIndex = 0 }
  let slope4 = Slope { count = 0, lineShift = 1, colShift = 7, lineIndex = 0, colIndex = 0 }
  let slope5 = Slope { count = 0, lineShift = 2, colShift = 1, lineIndex = 0, colIndex = 0 }

  let slopes = [slope1, slope2, slope3, slope4, slope5]
  let counts = countTrees list slopes 0
  print $  foldl (*) 1 counts