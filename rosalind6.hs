import Data.List
import Char

main = do
     l <- getContents
     let l' = myToInt l
     let list = getList l'
     let perms = getPermutations list
     putStrLn $ show (length perms)
     printing perms
     return ()

getPermutations :: [Integer] -> [[Integer]]
getPermutations x = permutations x


getList :: Integer -> [Integer]
getList 0 =[]
getList x = x:(getList (x-1))

myToInt :: [Char] -> Integer
myToInt x = read x :: Integer

printing :: [[Integer]] -> IO ()
printing [] = do
         return ()
printing (x:xs) = do
         let string = formString x
         putStrLn string
         let z = printing xs
         seq z z
         return ()
         

formString :: [Integer] -> String
formString [] = ""
formString (x:xs) = [chr((mytoint(x))+48)] ++ " " ++ formString(xs)

mytoint :: Integer -> Int
mytoint x = fromIntegral x