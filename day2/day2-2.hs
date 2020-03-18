import Data.List.Split
import System.Environment

main = do
    args <- getArgs
    contents <- readFile $ head args
    let program = map read $ splitOn "," contents :: [Int]
    print $ tryNounVerb 0 0 program

tryNounVerb :: Int -> Int -> [Int] -> Int
tryNounVerb noun verb program
  | output == 19690720 = (100 * noun) + verb
  | noun > 99 = error "Noun out of range"
  | verb > 99 = tryNounVerb (noun + 1) 0 program
  | otherwise = tryNounVerb noun (verb + 1) program
  where output = parseProg 0 $ setNounVerb noun verb program

setNounVerb :: Int -> Int -> [Int] -> [Int]
setNounVerb noun verb program = replaceAt 2 verb $ replaceAt 1 noun program
    
parseProg :: Int -> [Int] -> Int
parseProg instPtr program
  | opcode == 1 = parseProg (instPtr + 4) $ add program instPtr
  | opcode == 2 = parseProg (instPtr + 4) $ mult program instPtr
  | opcode == 99 = head program
  | otherwise = error $ "Invalid opcode: " ++ show opcode ++ " Instruction pointer: " ++ show instPtr
  where opcode = program !! instPtr

add :: [Int] -> Int -> [Int]
add program instPtr = do
  let a = program !! (program !! (instPtr + 1))
  let b = program !! (program !! (instPtr + 2))
  let result = a + b
  let outPos = program !! (instPtr + 3)
  replaceAt outPos result program


mult :: [Int] -> Int -> [Int]
mult program instPtr = do
  let a = program !! (program !! (instPtr + 1))
  let b = program !! (program !! (instPtr + 2))
  let result = a * b
  let outPos = program !! (instPtr + 3)
  replaceAt outPos result program

replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt at with list = let (xs, ys) = splitAt at list in xs ++ [with] ++ (tail ys)
