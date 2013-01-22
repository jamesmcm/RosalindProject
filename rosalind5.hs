main = do
     l <- getContents
     let l' = lines l
     let x = head l'
     let y = last l'
     putStrLn $ show (sum ( map areDifferent (zip x y)))

areDifferent :: (Char,Char) -> Integer
areDifferent z
             | x==y = 0
             | otherwise = 1
             where x=fst(z)
                   y=snd(z)