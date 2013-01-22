main = do
     l <- getLine
     let l' = reverse l
     putStrLn $ (map getComplement l')


getComplement x
        | x == 'A' = 'T'
        | x == 'T' = 'A'
        | x == 'C' = 'G'
        | x == 'G' = 'C'
        | otherwise = x
