-- Get all permutations for length given
-- Calculate probability for each permutation

-- Can simplify to just two symbols {G,C}, {A,T}, then number is out by factor 2^n_sub
-- Calculate all different permutation probabilities, and their number out of total permutations
-- Calculate length of possible sliding window across big string (depends on both length) - (n-m)+1
-- So we have probability of event, and number of chances, want probability of occuring AT LEAST ONCE
-- So calculate probability of never occuring (1-P)^N, then 1-that
-- Then multiply this by the probability of the substring permutation, and sum with the other substrings


-- Can just use nCr directly when reduced to two element case - calculate number of cases with n {g,c}, m {a,t} and have probability - make list of tuples
-- nCr = n!/(k!(n-k!))
-- Reduce to function that can be mapped across gcs
-- (Multiply the probability of the substring by this - average number expected?)
   
main = do
     l1 <- getContents
     let l = lines l1
     let lengths1 = map myToInt $ words $ l!!0
     let gcs = map myToDouble $ words $ l!!1
     let sublength=lengths1!!0
     let fulllength=lengths1!!1
     putStrLn $ show $ expectednumber fulllength sublength $ map (\x -> x**2) ( genntuples sublength (gcs!!0))
     putStrLn $ show $ genntuples sublength (gcs!!0)
     putStrLn $ show $ map (finalfunction fulllength sublength) gcs

-- gomonad :: Integer -> Integer -> [Integer] -> [Double] -> IO ()
-- gomonad sublength fulllength (n:ns) gcs = do
--         let gc = gcs!!n
        
--         let z = gomonad sublength fullength ns gcs
        
        


--Define x as number of {G,C}s
genprobtuples :: Int -> Double -> [Double]
genprobtuples n gc = [((gc^x)*((1-gc)^(n-x))) | x<-[0..n]]

genntuples :: Int -> Double -> [Double]
genntuples n gc = [((gc**intToDouble(x))*((1-gc)**intToDouble(n-x)))*intToDouble(choose n x) | x<-[0..n]]
--Gives list of probabilities which sum to 1 of different microstates of substrings
-- Only gives weightings

-- *intToDouble(2^n)
probabilityonce :: Int -> Int -> [Double] -> [Double]
probabilityonce nfull nsub x = map (probhelp nfull nsub) x

probhelp :: Int -> Int -> Double -> Double
probhelp nfull nsub x = 1-((1-x)^fromIntegral((nfull-nsub)+1))


expectednumber :: Int -> Int -> [Double] -> [Double]
expectednumber nfull nsub x = map (expecthelp nfull nsub) x

expecthelp :: Int -> Int -> Double -> Double
expecthelp nfull nsub x = (x* intToDouble((nfull-nsub)+1) )

-- (nfull-nsub)+1)
-- fromIntegral( (nfull-(nfull `mod` nsub) ) `div` (nsub) ) 
finalfunction :: Int -> Int -> Double -> Double
finalfunction nfull nsub gc = sum $ zipWith (*) ntuples (expectednumber nfull nsub $ map (\x -> x**2) ntuples)
              where probtuples = genprobtuples nsub gc
                    ntuples = genntuples nsub gc
-- zipWith (*) probtuples

choose :: Int -> Int -> Int
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 

myToInt :: String -> Int
myToInt x = read x

myToDouble :: String -> Double
myToDouble x = read x

intToDouble :: Int -> Double
intToDouble x = fromIntegral x


--Sliding window for test number is (n-m)+1
getNList :: Int -> String -> [String]
getNList _ [] = []
getNList n x = take n x : getNList n (drop 1 x)