import System.Environment

main = do
    args <- getArgs
    content <- readFile(args !! 0)
