import System.IO  

f :: [String] -> [Integer]
f = map read

fuel2:: Integer -> Integer
fuel2 cargo 
   | cargo <= 6 = 0
   | otherwise = let need = ((div cargo  3) - 2) in
         (fuel2 need) + need
  
fuel:: Integer -> Integer
fuel x = (div x  3) - 2 
  
main = do  
    handle <- openFile "data.txt" ReadMode  
    contents <- hGetContents handle  
    print $ sum $ map fuel $ f $ lines contents
    print $ sum $ map fuel2 $ f $ lines contents
    hClose handle  
