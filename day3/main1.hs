import System.Environment
import Data.List.Split
import Data.List

main = do
    args <- getArgs
    content <- readFile(args !! 0)
    let directions = lines content
    let d_first = splitOn "," $ directions!!0
    let d_second = splitOn "," $ directions!!1

    let visited1 = getVisited d_first
    let visited2 = getVisited d_second

    let crossings = intersect visited1 visited2

    print $ minimum $ map calcToOrigin crossings


type Coord = (Int, Int)


getVisited :: [String] -> [Coord]
getVisited x = getVisited' [] x


getVisited' :: [Coord] -> [String] -> [Coord]
getVisited' coords [] = coords
getVisited' [] (x:xs) = getVisited' (getCoordsForCommand x) xs
getVisited' coords (x:xs) = do
    let nullCoords = getCoordsForCommand x
    let newCoords = map (tupleAdd (last coords)) nullCoords
    getVisited' (coords ++ newCoords) xs


getCoordsForCommand :: String -> [Coord]
getCoordsForCommand (x:xs)
    | x == 'U' = [(i,j) | i <- [0], j <- [1..end]]
    | x == 'D' = [(i,j) | i <- [0], j <- reverse [(-end)..(-1)]]
    | x == 'L' = [(i,j) | j <- [0], i <- reverse [(-end)..(-1)]]
    | x == 'R' = [(i,j) | j <- [0], i <- [1..end]]
    where end = read xs


calcToOrigin :: Coord -> Int
calcToOrigin (x, y) = (abs x) + (abs y)


tupleAdd :: Coord -> Coord -> Coord
tupleAdd a b = ((fst a + fst b), (snd a + snd b))
