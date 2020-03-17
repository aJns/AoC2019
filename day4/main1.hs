import System.Environment
import Data.Char
import Data.List
import Data.List.Split

main = do
    args <- getArgs
    content <- readFile(args !! 0)
    let ranges = lines content
    let counts = map countPossible ranges
    print counts


countPossible :: String -> Int
countPossible str = length $ genPasses high (low - 1) []
    where (low, high) = parseRange str


parseRange :: String -> (Int, Int)
parseRange str =
    (nums!!0, nums!!1)
        where nums = map read $ splitOn "-" str


genPasses :: Int -> Int -> [Int] -> [Int]
genPasses high curr out
  | curr < high = genPasses high next $ addToList next out
  | otherwise = out
  where next = curr + 1


addToList :: Int -> [Int] -> [Int]
addToList candidate toList
  | isValid candidate = toList ++ [candidate]
  | otherwise = toList


isValid :: Int -> Bool
isValid a = (isNonDecreasing a) && (hasPair $ digits a) -- This short circuits (on the first) so yay


isNonDecreasing :: Int -> Bool
isNonDecreasing a = isSorted $ digits a


hasPair :: [Int] -> Bool
hasPair [] = False
hasPair [x] = False
hasPair (x:y:xs) = x == y || hasPair (y:xs)


digits :: Int -> [Int]
digits n = [digitToInt x | x <- show n]


isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)
