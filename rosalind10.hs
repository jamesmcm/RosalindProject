import Data.List

main = do
     l <- getContents
     let l' = lines l
     putStrLn $ (getConsensus(getProfile l'))
     putStrLn $ getMatrix( (getProfile l'))

getMatrix :: [(Int, Int, Int, Int)] -> String
getMatrix x = parseTuple(intTupleToStringTuple(foldl matrixfold ([],[],[],[]) x))

-- Could probably be replaced with map of append across tuple
matrixfold :: ([Int], [Int], [Int], [Int]) -> (Int, Int, Int, Int) -> ([Int], [Int], [Int], [Int])
matrixfold (al,cl,gl,tl) (a,c,g,t) = (al++[a], cl++[c], gl++[g], tl++[t])

parseTuple :: (String, String, String, String) -> String
parseTuple (a,c,g,t) = a++"\n"++c++"\n"++g++"\n"++t

intTupleToStringTuple :: ([Int], [Int], [Int], [Int]) -> (String, String, String, String)
intTupleToStringTuple (a,c,g,t) = (listToString 'A' a, listToString 'C' c, listToString 'G' g, listToString 'T' t)

listToString :: Char -> [Int] -> String
listToString c x = foldl (\x y -> x ++ " " ++ (show y) ) ([c]++[':']) x

getConsensus :: [(Int, Int, Int, Int)] -> String
getConsensus s = foldl (\x y -> x++ [(getMostCommonLetter y)]) "" (map intTupleToList s)

getMostCommonLetter :: [Int] -> Char
getMostCommonLetter x
                    | maxx == 0 = 'A'
                    | maxx == 1 = 'C'
                    | maxx == 2 = 'G'
                    | maxx == 3 = 'T'
                    where maxx = escapemaybe (elemIndex (maximum x) x)
--Note returns first index in case of tie

intTupleToList :: (Int,Int,Int,Int) -> [Int]
intTupleToList (a,b,c,d) = [a]++[b]++[c]++[d]

getProfile :: [String] -> [(Int,Int,Int,Int)]
getProfile [] = []
getProfile x
           | head x == [] = []
           | otherwise = [getTuple x] ++ (getProfile (map (drop 1) x))

getTuple :: [String] -> (Int, Int, Int, Int)
getTuple x = foldl foldfunc (0,0,0,0) (map head x)

foldfunc :: (Int, Int, Int, Int) -> Char -> (Int, Int, Int, Int)
foldfunc (a,c,g,t) x
         | x=='A' = (a+1,c,g,t)
         | x=='C' = (a,c+1,g,t)
         | x=='G' = (a,c,g+1,t)
         | x=='T' = (a,c,g,t+1)

escapemaybe :: Maybe Int -> Int
escapemaybe (Just x) = x
escapemaybe Nothing = 0