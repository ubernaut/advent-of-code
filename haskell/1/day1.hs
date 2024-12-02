import Data.List (sort)
import System.IO
processInput :: String -> ([Int], [Int])
processInput content = 
    let rows = lines content
        pairs = map (words) rows
        leftNums = map (read . head) pairs :: [Int]
        rightNums = map (read . last) pairs :: [Int]
    in (leftNums, rightNums)

calculateDifference :: [Int] -> [Int] -> Int
calculateDifference xs ys = sum $ map getDiff (zip xs ys)
  where
    getDiff (x, y) = abs (x - y) -- abs ensures we always get positive difference

calculateSimilarityScore :: [Int] -> [Int] -> Int
calculateSimilarityScore leftList rightList = sum $ map scoreNumber leftList
  where
    scoreNumber x = x * (countOccurrences x rightList)
    countOccurrences n = length . filter (== n)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (leftColumn, rightColumn) = processInput content
    
    let sortedLeft = sort leftColumn
    let sortedRight = sort rightColumn

    let difference = calculateDifference sortedLeft sortedRight
    putStrLn "diff:"
    print difference

    let score = calculateSimilarityScore sortedLeft sortedRight
    putStrLn "score:"
    print score