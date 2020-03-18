import Data.List.Split
import System.Environment

main = do
    args <- getArgs
    contents <- readFile $ head args
    let program = map read $ splitOn "," contents :: [Int]
    print $ parseProg 0 program
    
parseProg :: [Int] -> Int -> Int
parseProg instPtr program
  | opcode == 1 = parseProg (instPtr + 4) $ add program instPtr
  | opcode == 2 = parseProg (instPtr + 4) $ mult program instPtr
  | opcode == 99 = head program
  | otherwise = error "Invalid opcode"
  where opcode = program !! instPtr

add :: [Int] -> Int -> [Int]

mult :: [Int] -> Int -> [Int]
