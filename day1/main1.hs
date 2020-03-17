import System.Environment

main = do
    args <- getArgs
    content <- readFile(args !! 0)
    let masses = lines content
    putStrLn $ show $ sum $ map calcFuel $ map read masses


calcFuel :: Int -> Int
calcFuel mass = (div mass 3) - 2
