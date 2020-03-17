import System.Environment

main = do
    args <- getArgs
    content <- readFile(args !! 0)
    let masses = map read $ lines content
    print $ sum $ concat $ map calcFuelStart masses


calcFuel :: Int -> Int
calcFuel mass = max ((div mass 3) - 2) 0


calcFuelFuel :: [Int] -> [Int]
calcFuelFuel [x] = calcFuelFuel ((calcFuel x):[x])
calcFuelFuel (x:xs)
  | x > 0       = calcFuelFuel ((calcFuel x):x:xs)
  | otherwise   = xs


calcFuelStart :: Int -> [Int]
calcFuelStart x = calcFuelFuel [(calcFuel x)]
