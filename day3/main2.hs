import System.Environment
import Data.List.Split
import Data.List
import Data.Maybe

main = do
    args <- getArgs
    content <- readFile(args !! 0)
    let directions = lines content
    let d_first = splitOn "," $ directions!!0
    let d_second = splitOn "," $ directions!!1

    let visited1 = getVisited d_first
    let visited2 = getVisited d_second

    let crossings = intersect visited1 visited2
    let indices = map (getCrossIndices (visited1, visited2)) crossings

    print $ minimum $ map tupleSum indices


type Coord = (Int, Int)


getCrossIndices :: ([Coord], [Coord]) -> Coord -> (Int, Int)
getCrossIndices (a, b) c = do
    let a_ind = (fromJust $ elemIndex c a) + 1 -- Haskell is zero indexed
    let b_ind = (fromJust $ elemIndex c b) + 1 -- We want starting from 1
    (a_ind, b_ind)


tupleSum :: (Int, Int) -> Int
tupleSum (a, b) = a + b


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
