import Control.Monad  
import Data.Char 

main = do
     l <- getLine
     putStrLn $ show ( foldr myfunction (0, 0, 0, 0) l)


myfunction :: Char -> (Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer)
myfunction x (a,c,g,t)
           | x=='A' = (a+1,c,g,t)
           | x=='C' = (a,c+1,g,t)
           | x=='G' = (a,c,g+1,t)
           | x=='T' = (a,c,g,t+1)
           | otherwise = (a,c,g,t)
