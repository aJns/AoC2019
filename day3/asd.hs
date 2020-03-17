type Coord = (Int, Int)


calcToOrigin :: Coord -> Int
calcToOrigin (x, y) = (abs x) + (abs y)
