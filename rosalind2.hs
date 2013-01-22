main = do
     l <- getLine
     putStrLn $ (map myfunction l)

myfunction x
        | x == 'T' = 'U'
        | otherwise = x
