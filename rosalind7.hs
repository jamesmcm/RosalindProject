import Text.Printf

main = do
     l <- getContents
     let l' = map mytoDouble(words l)
     let problist = map getProb l'
     let probliststr = map mytoStr problist
     let finalstr = foldl foldfunc [] probliststr
     putStrLn finalstr
     return ()

mytoDouble :: String -> Double
mytoDouble x = read x :: Double

mytoStr :: Double -> String
mytoStr x = printf "%.6f" (x :: Double)

getProb :: Double -> Double
getProb x = ((2*((x/2)*(x/2))) + (2*((1-x)/2)*((1-x)/2)))


foldfunc :: String -> String -> String
foldfunc x y = x++" "++y