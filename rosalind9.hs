import List

main = do
     l <- getContents
     let l' = lines l
     let sub = last l'
     let str = head l'
     let lensub = length sub
     putStrLn $ formString(( ( snd(getPositions sub (getNList (lensub) (cropString sub str))))))

cropString :: String -> String -> String
cropString sub x = take ((length x) - ((length x)`mod` (length sub))) x

getNList :: Int -> String -> [String]
getNList _ [] = []
getNList n x = take n x : getNList n (drop 1 x)

getPositions :: String -> [String] -> (Int, [Int])
getPositions _ [] = (1, [])
getPositions sub str = foldl (foldfunc sub) (1, []) str


foldfunc :: String -> (Int, [Int]) -> String -> (Int, [Int])
foldfunc _ _ [] = (1, [])
foldfunc sub (acc, indexlist) str
         | sub == str = (acc+1, indexlist++[acc])
         | otherwise = (acc+1, indexlist)

formString :: [Int] -> String
formString [] = []
formString (x:xs) = (show x) ++ " " ++ (formString xs)