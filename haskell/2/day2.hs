import Data.List (words)
import Data.List (tails)
processInput :: String -> [[Int]]
processInput content = 
    let rows = lines content  -- Split into lines
        nonEmptyRows = filter (not . null) rows  -- Remove empty lines
        numberLists = map words nonEmptyRows  -- Split each line into strings of numbers
        intLists = map (map read) numberLists :: [[Int]]  -- Convert each string to Int
    in intLists
-- Check if adjacent numbers differ by 1 to 3
validDifference :: Int -> Int -> Bool
validDifference x y = let diff = abs (x - y) in diff >= 1 && diff <= 3

-- Check if all adjacent pairs in a list have valid differences
allValidDifferences :: [Int] -> Bool
allValidDifferences xs = all (uncurry validDifference) $ zip xs (tail xs)

-- Check if list is strictly increasing
isIncreasing :: [Int] -> Bool
isIncreasing xs = and $ zipWith (<) xs (tail xs)

-- Check if list is strictly decreasing
isDecreasing :: [Int] -> Bool
isDecreasing xs = and $ zipWith (>) xs (tail xs)

-- Check if a list is monotonic (either all increasing or all decreasing)
isMonotonic :: [Int] -> Bool
isMonotonic xs = isIncreasing xs || isDecreasing xs

-- Check if a list is "safe" without removing elements
isDirectlySafe :: [Int] -> Bool
isDirectlySafe xs 
    | length xs <= 1 = False
    | otherwise = isMonotonic xs && allValidDifferences xs

-- Remove one element at a time and check if list becomes safe
canBeMadeSafe :: [Int] -> Bool
canBeMadeSafe xs
    | length xs <= 2 = False  -- Need at least 3 elements to remove one and have a valid list
    | isDirectlySafe xs = True
    | otherwise = any isDirectlySafe (removeOneAtATime xs)
  where
    removeOneAtATime lst = [take i lst ++ drop (i+1) lst | i <- [0..length lst - 1]]

-- Analyze all lists and count those that are safe or can be made safe
analyzeSafety :: [[Int]] -> (Int, Int, [Int])
analyzeSafety lists = 
    let directlySafe = length $ filter isDirectlySafe lists
        canBeSafe = length $ filter canBeMadeSafe lists
        safeIndices = [i | (list, i) <- zip lists [0..], canBeMadeSafe list]
    in (directlySafe, canBeSafe, safeIndices)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let lists = processInput content
    let (directlySafe, canBeSafe, safeIndices) = analyzeSafety lists
    putStrLn $ "Number of directly safe lists: " ++ show directlySafe
    putStrLn $ "Number of lists that are safe or can be made safe: " ++ show canBeSafe
    putStrLn $ "Indices of safe or can-be-made-safe lists: " ++ show safeIndices