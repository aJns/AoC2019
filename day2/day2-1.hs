import Data.List.Split
import System.Environment

main = do
    args <- getArgs
    contents <- readFile $ head args
    let program = map read $ splitOn "," contents :: [Int]
    print program
    
