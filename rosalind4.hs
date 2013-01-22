import Data.List

--Data.List elemIndex :: Eq a => a -> [a] -> Maybe Int

main = do
     l <- getContents
     let l' = lines l
     -- putStrLn $ (show l')
     let l'' = fixlist l'
     -- putStrLn $ (show l'')
     let labellist = filter (\x -> (head x == '>')) l''
     let stringlist = filter (\x -> not (head x == '>')) l''
     let gclist = getgclist stringlist
     let maxgclist = maximum gclist
     let idnum = escapemaybe(elemIndex maxgclist gclist)
     putStrLn $ (drop 1 (labellist!!idnum))
     putStrLn $ (show (maxgclist*100)) ++ "%"


-- filterinput :: [Char] -> [[Char]]
-- filterinput [] = []
-- filterinput x = fst(splitAt ind):filterinput(snd(helper))
--             where indices = escapemaybe(elemIndex '>' x)


fixlist :: [[Char]] -> [[Char]]
fixlist [] = []
fixlist (x:[]) = [x]
fixlist (x:y:end)
        | ((head x == '>')  || (head y == '>')) = x:(fixlist (y:end))
        | otherwise = (fixlist ((x++y):end))





-- removechar :: Char -> [Char] -> [Char]
-- removechar c [] = []
-- removechar c (x:xs)
--             | (c==x) = removechar c xs
--             | otherwise = x:removechar c xs

getgclist :: [[Char]] -> [Double]
getgclist x = map getgcContent x

getgcContent :: [Char] -> Double
getgcContent x = (mytoDouble2((mycount 'G' x) + (mycount 'C' x)))/(mytoDouble(length x))

mycount :: (Eq a) => a -> [a] -> Integer
mycount c x = sum ( map (counthelp c) x)

counthelp :: (Eq a) => a -> a -> Integer
counthelp c x
          | c==x = 1
          | otherwise = 0

escapemaybe :: Maybe Int -> Int
escapemaybe (Just x) = x
escapemaybe Nothing = 0

escapemaybe' :: Maybe [Int] -> [Int]
escapemaybe' (Just x) = x
escapemaybe' Nothing = []

mytoDouble :: Int -> Double
mytoDouble x = fromIntegral x

mytoDouble2 :: Integer -> Double
mytoDouble2 x = fromIntegral x